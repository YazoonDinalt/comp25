(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringSet = Set.Make (String)

let rec pat_vars = function
  | PatVar (name, _) -> StringSet.singleton name
  | PatTuple ps -> List.fold_left StringSet.union StringSet.empty (List.map pat_vars ps)
  | _ -> StringSet.empty
;;

let rec free_vars = function
  | ExpConst _ -> StringSet.empty
  | ExpVar (name, _) -> StringSet.singleton name
  | ExpBinaryOp (_, e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | ExpApp (f, arg, _) -> StringSet.union (free_vars f) (free_vars arg)
  | ExpIfElse (c, t, e) ->
    StringSet.union (free_vars c) (StringSet.union (free_vars t) (free_vars e))
  | ExpLetIn (_, name, e1, e2) ->
    StringSet.union (free_vars e1) (StringSet.remove name (free_vars e2))
  | ExpLetPatIn (pat, e1, e2) ->
    StringSet.union (free_vars e1) (StringSet.diff (free_vars e2) (pat_vars pat))
  | ExpTuple es -> List.fold_left StringSet.union StringSet.empty (List.map free_vars es)
  | ExpFun (PatVar (name, _), body) -> StringSet.remove name (free_vars body)
  | ExpFun (_, body) -> free_vars body
  | _ -> StringSet.empty
;;

let rec cc_expr bound = function
  | (ExpConst _ | ExpVar _) as e -> e
  | ExpBinaryOp (op, e1, e2) -> ExpBinaryOp (op, cc_expr bound e1, cc_expr bound e2)
  | ExpApp (f, arg, t) -> ExpApp (cc_expr bound f, cc_expr bound arg, t)
  | ExpIfElse (c, t, e) -> ExpIfElse (cc_expr bound c, cc_expr bound t, cc_expr bound e)
  | ExpLetIn (rf, name, e1, e2) ->
    ExpLetIn (rf, name, cc_expr bound e1, cc_expr (StringSet.add name bound) e2)
  | ExpLetPatIn (pat, e1, e2) ->
    ExpLetPatIn (pat, cc_expr bound e1, cc_expr (StringSet.union (pat_vars pat) bound) e2)
  | ExpTuple es -> ExpTuple (List.map (cc_expr bound) es)
  | ExpFun _ as lambda ->
    let captured = StringSet.inter (free_vars lambda) bound |> StringSet.elements in
    let closed = cc_fun bound lambda in
    let wrapped =
      List.fold_right
        (fun fv body -> ExpFun (PatVar (fv, TypeUnknown), body))
        captured
        closed
    in
    List.fold_left
      (fun f fv -> ExpApp (f, ExpVar (fv, TypeUnknown), TypeUnknown))
      wrapped
      captured
  | e -> e

and cc_fun bound = function
  | ExpFun (pat, body) ->
    let bound' =
      match pat with
      | PatVar (name, _) -> StringSet.add name bound
      | _ -> bound
    in
    ExpFun (pat, cc_fun bound' body)
  | e -> cc_expr bound e
;;

let cc_binding = function
  | Let (rf, pats) ->
    Let (rf, List.map (fun (pat, body) -> pat, cc_expr StringSet.empty body) pats)
  | b -> b
;;

let cc_program stmts = List.map cc_binding stmts

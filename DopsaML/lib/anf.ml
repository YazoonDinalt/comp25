(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type 'a m = int -> 'a * int

let return x : 'a m = fun s -> x, s

let ( let* ) (m : 'a m) (f : 'a -> 'b m) : 'b m =
  fun s ->
  let x, s' = m s in
  f x s'
;;

let fresh : string m = fun s -> Printf.sprintf "anf%d" s, s + 1
let run (m : 'a m) : 'a = fst (m 0)

let is_atom = function
  | ExpConst _ | ExpVar _ -> true
  | _ -> false
;;

let rec collect_app_chain = function
  | ExpApp (f, arg, t) ->
    let func, args = collect_app_chain f in
    func, args @ [ arg, t ]
  | e -> e, []
;;

let rec anf_expr expr (k : expression -> expression m) : expression m =
  match expr with
  | ExpConst _ | ExpVar _ -> k expr
  | ExpFun (pat, body) ->
    let* body_anf = anf_body body in
    k (ExpFun (pat, body_anf))
  | ExpBinaryOp (op, e1, e2) ->
    anf_atom e1 (fun a1 -> anf_atom e2 (fun a2 -> k (ExpBinaryOp (op, a1, a2))))
  | ExpApp _ as app ->
    let func, args = collect_app_chain app in
    let norm_func cont =
      match func with
      | ExpVar _ -> cont func
      | ExpFun (pat, body) ->
        let* body_anf = anf_body body in
        cont (ExpFun (pat, body_anf))
      | _ -> anf_atom func cont
    in
    norm_func (fun af ->
      anf_atom_list args (fun anf_args ->
        let rebuilt = List.fold_left (fun f (arg, t) -> ExpApp (f, arg, t)) af anf_args in
        k rebuilt))
  | ExpIfElse (cond, then_, else_) ->
    anf_atom cond (fun ac ->
      let* then_anf = anf_body then_ in
      let* else_anf = anf_body else_ in
      k (ExpIfElse (ac, then_anf, else_anf)))
  | ExpLetIn (rf, name, e1, e2) ->
    anf_expr e1 (fun v1 ->
      let* e2_anf = anf_expr e2 k in
      return (ExpLetIn (rf, name, v1, e2_anf)))
  | ExpLetPatIn (pat, e1, e2) ->
    anf_expr e1 (fun v1 ->
      let* e2_anf = anf_expr e2 k in
      return (ExpLetPatIn (pat, v1, e2_anf)))
  | e -> k e

and anf_body expr : expression m = anf_expr expr return

and anf_atom expr (k : expression -> expression m) : expression m =
  match expr with
  | ExpConst _ | ExpVar _ -> k expr
  | ExpFun (pat, body) ->
    let* body_anf = anf_body body in
    k (ExpFun (pat, body_anf))
  | _ ->
    anf_expr expr (fun v ->
      if is_atom v
      then k v
      else
        let* tmp = fresh in
        let* rest = k (ExpVar (tmp, TypeUnknown)) in
        return (ExpLetIn (Notrec, tmp, v, rest)))

and anf_atom_list lst (k : (expression * type_of_var) list -> expression m) : expression m
  =
  match lst with
  | [] -> k []
  | (arg, t) :: rest ->
    anf_atom arg (fun a -> anf_atom_list rest (fun rest_anf -> k ((a, t) :: rest_anf)))
;;

let anf_binding = function
  | Let (rf, pats) ->
    List.map (fun (pat, expr) -> pat, run (anf_body expr)) pats
    |> fun pats -> Let (rf, pats)
  | Exp e -> Exp (run (anf_body e))
;;

let anf_program stmts = List.map anf_binding stmts

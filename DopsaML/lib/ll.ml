(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec ll_expr counter = function
  | (ExpConst _ | ExpVar _) as e -> e, [], counter
  | ExpBinaryOp (op, e1, e2) ->
    let e1', bs1, c1 = ll_expr counter e1 in
    let e2', bs2, c2 = ll_expr c1 e2 in
    ExpBinaryOp (op, e1', e2'), bs1 @ bs2, c2
  | ExpApp (f, arg, t) ->
    let f', bs1, c1 = ll_expr counter f in
    let arg', bs2, c2 = ll_expr c1 arg in
    ExpApp (f', arg', t), bs1 @ bs2, c2
  | ExpIfElse (c, t, e) ->
    let c', bs0, c0 = ll_expr counter c in
    let t', bs1, c1 = ll_expr c0 t in
    let e', bs2, c2 = ll_expr c1 e in
    ExpIfElse (c', t', e'), bs0 @ bs1 @ bs2, c2
  | ExpLetIn (rf, name, e1, e2) ->
    let e1', bs1, c1 = ll_expr counter e1 in
    let e2', bs2, c2 = ll_expr c1 e2 in
    ExpLetIn (rf, name, e1', e2'), bs1 @ bs2, c2
  | ExpLetPatIn (pat, e1, e2) ->
    let e1', bs1, c1 = ll_expr counter e1 in
    let e2', bs2, c2 = ll_expr c1 e2 in
    ExpLetPatIn (pat, e1', e2'), bs1 @ bs2, c2
  | ExpTuple es ->
    let es', bs, c =
      List.fold_left
        (fun (acc, bs, c) e ->
           let e', bs', c' = ll_expr c e in
           acc @ [ e' ], bs @ bs', c')
        ([], [], counter)
        es
    in
    ExpTuple es', bs, c
  | ExpFun _ as lambda ->
    let name = Printf.sprintf "ll%d" counter in
    let lifted, inner_bs, c1 = ll_fun (counter + 1) lambda in
    ExpVar (name, TypeUnknown), inner_bs @ [ name, lifted ], c1
  | e -> e, [], counter

and ll_fun counter = function
  | ExpFun (pat, body) ->
    let body', bs, c = ll_fun counter body in
    ExpFun (pat, body'), bs, c
  | e -> ll_expr counter e
;;

let ll_binding counter = function
  | Let (rf, [ (pat, body) ]) ->
    let body', new_binds, counter' = ll_fun counter body in
    let lifted =
      List.map (fun (n, e) -> Let (Notrec, [ PatVar (n, TypeUnknown), e ])) new_binds
    in
    lifted @ [ Let (rf, [ pat, body' ]) ], counter'
  | b -> [ b ], counter
;;

let ll_program stmts =
  let _, result =
    List.fold_left
      (fun (counter, acc) binding ->
         let new_bindings, counter' = ll_binding counter binding in
         counter', acc @ new_bindings)
      (0, [])
      stmts
  in
  result
;;

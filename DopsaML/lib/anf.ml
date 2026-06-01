(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* ANF: every operand is an immediate, every intermediate result is named *)

type imm =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmVar of string

type cexpr =
  | CImm of imm
  | CBinop of binary_op * imm * imm
  | CApp of string * imm list (* call of a named function/closure *)
  | CIf of imm * aexpr * aexpr
  | CTuple of imm list
  | CField of imm * int (* i-th field of a tuple *)

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

type afunc =
  { name : string
  ; is_rec : bool
  ; params : string list
  ; body : aexpr
  }

type aprogram = afunc list

(* state (fresh-name counter) + error monad *)
type 'a m = int -> ('a, string) result * int

let return x : 'a m = fun s -> Ok x, s
let fail msg : 'a m = fun s -> Error msg, s

let ( let* ) (m : 'a m) (f : 'a -> 'b m) : 'b m =
  fun s ->
  match m s with
  | Ok x, s' -> f x s'
  | (Error _ as e), s' -> e, s'
;;

let fresh : string m = fun s -> Ok (Printf.sprintf "anf%d" s), s + 1
let run (m : 'a m) = fst (m 0)

let rec unwrap_params = function
  | ExpFun (PatVar (name, _), rest) ->
    let params, inner = unwrap_params rest in
    name :: params, inner
  | e -> [], e
;;

(* bind a pattern's pieces (the value is v) around rest *)
let rec bind_pat pat v rest =
  match pat with
  | PatVar (x, _) -> return (ALet (x, CImm v, rest))
  | PatWild | PatConst _ -> return rest
  | PatTuple ps ->
    let rec go i = function
      | [] -> return rest
      | p :: tl ->
        let* inner = go (i + 1) tl in
        bind_field p v i inner
    in
    go 0 ps
  | PatCon _ -> fail "ANF: unsupported pattern"

(* bind sub-pattern p = i-th field of v, around rest *)
and bind_field p v i rest =
  match p with
  | PatVar (x, _) -> return (ALet (x, CField (v, i), rest))
  | PatWild | PatConst _ -> return rest
  | PatTuple _ ->
    let* t = fresh in
    let* inner = bind_pat p (ImmVar t) rest in
    return (ALet (t, CField (v, i), inner))
  | PatCon _ -> fail "ANF: unsupported pattern"
;;

(* e -> its value as an immediate, passed to k *)
let rec anf_imm e (k : imm -> aexpr m) : aexpr m =
  match e with
  | ExpConst (ConstInt n) -> k (ImmInt n)
  | ExpConst (ConstBool b) -> k (ImmBool b)
  | ExpConst ConstNil -> k ImmUnit
  | ExpVar ("()", _) -> k ImmUnit
  | ExpVar (x, _) -> k (ImmVar x)
  | _ ->
    let* t = fresh in
    let* rest = k (ImmVar t) in
    anf_cexpr e (fun c -> return (ALet (t, c, rest)))

(* same, but forces a var name (the callee has to be one) *)
and anf_imm_var e (k : string -> aexpr m) : aexpr m =
  anf_imm e (fun i ->
    match i with
    | ImmVar x -> k x
    | _ ->
      let* t = fresh in
      let* rest = k t in
      return (ALet (t, CImm i, rest)))

and anf_imm_list es (k : imm list -> aexpr m) : aexpr m =
  match es with
  | [] -> k []
  | e :: rest -> anf_imm e (fun i -> anf_imm_list rest (fun is -> k (i :: is)))

and anf_cexpr e (k : cexpr -> aexpr m) : aexpr m =
  match e with
  | ExpConst _ | ExpVar _ -> anf_imm e (fun i -> k (CImm i))
  | ExpBinaryOp (op, l, r) ->
    anf_imm l (fun il -> anf_imm r (fun ir -> k (CBinop (op, il, ir))))
  | ExpIfElse (c, t, e) ->
    anf_imm c (fun ic ->
      let* t' = anf_aexpr t in
      let* e' = anf_aexpr e in
      k (CIf (ic, t', e')))
  | ExpLetIn (_, name, e1, e2) ->
    anf_cexpr e1 (fun c1 ->
      let* rest = anf_cexpr e2 k in
      return (ALet (name, c1, rest)))
  | ExpLetPatIn (pat, e1, e2) ->
    anf_cexpr e1 (fun c1 ->
      let* t = fresh in
      let* rest = anf_cexpr e2 k in
      let* bound = bind_pat pat (ImmVar t) rest in
      return (ALet (t, c1, bound)))
  | ExpTuple es -> anf_imm_list es (fun imms -> k (CTuple imms))
  | ExpApp _ ->
    let rec collect acc = function
      | ExpApp (f, arg, _) -> collect (arg :: acc) f
      | f -> f, acc
    in
    let head, args = collect [] e in
    anf_imm_var head (fun f -> anf_imm_list args (fun iargs -> k (CApp (f, iargs))))
  | _ -> fail "ANF: unsupported expression"

(* tail position: e's value becomes the aexpr's value *)
and anf_aexpr e : aexpr m = anf_cexpr e (fun c -> return (ACExpr c))

let anf_func = function
  | Let (rf, [ (PatVar (name, _), body) ]) ->
    let params, inner = unwrap_params body in
    let is_rec =
      match rf with
      | Rec -> true
      | Notrec -> false
    in
    (match run (anf_aexpr inner) with
     | Ok body -> Ok (Some { name; is_rec; params; body })
     | Error e -> Error e)
  | _ -> Ok None
;;

let anf_program stmts =
  List.fold_left
    (fun acc stmt ->
       match acc with
       | Error _ as e -> e
       | Ok funcs ->
         (match anf_func stmt with
          | Error _ as e -> e
          | Ok None -> Ok funcs
          | Ok (Some f) -> Ok (funcs @ [ f ])))
    (Ok [])
    stmts
;;

(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let ( let* ) = Result.bind
let context = Llvm.global_context ()
let builder = Llvm.builder context
let the_module = Llvm.create_module context "DopsaML"
let i64_t = Llvm.i64_type context
let i32_t = Llvm.i32_type context
let i1_t = Llvm.i1_type context

type env = (string, Llvm.llvalue) Hashtbl.t

let new_env () : env = Hashtbl.create 16
let env_set (env : env) name v = Hashtbl.replace env name v

let env_get (env : env) name =
  match Hashtbl.find_opt env name with
  | Some v -> Ok v
  | None -> Error (Printf.sprintf "Unbound variable: %s" name)
;;

let lookup_func name =
  match Llvm.lookup_function name the_module with
  | Some f -> Ok f
  | None -> Error (Printf.sprintf "Unknown function: %s" name)
;;

let map_result f lst =
  List.fold_right
    (fun x acc ->
       let* acc = acc in
       let* v = f x in
       Ok (v :: acc))
    lst
    (Ok [])
;;

let ensure_i1 v =
  if Llvm.type_of v = i1_t
  then v
  else Llvm.build_icmp Llvm.Icmp.Ne v (Llvm.const_int i64_t 0) "tobool" builder
;;

let rec codegen_expr (env : env) (func : Llvm.llvalue) = function
  | ExpConst (ConstInt n) -> Ok (Llvm.const_int i64_t n)
  | ExpConst (ConstBool b) -> Ok (Llvm.const_int i64_t (if b then 1 else 0))
  | ExpVar (name, _) -> env_get env name
  | ExpBinaryOp (op, e1, e2) ->
    let* l = codegen_expr env func e1 in
    let* r = codegen_expr env func e2 in
    Ok
      (match op with
       | Add -> Llvm.build_add l r "add" builder
       | Sub -> Llvm.build_sub l r "sub" builder
       | Mul -> Llvm.build_mul l r "mul" builder
       | Div -> Llvm.build_sdiv l r "div" builder
       | Eq -> Llvm.build_icmp Llvm.Icmp.Eq l r "eq" builder
       | Neq -> Llvm.build_icmp Llvm.Icmp.Ne l r "neq" builder
       | Less -> Llvm.build_icmp Llvm.Icmp.Slt l r "lt" builder
       | Gre -> Llvm.build_icmp Llvm.Icmp.Sgt l r "gt" builder
       | Leq -> Llvm.build_icmp Llvm.Icmp.Sle l r "leq" builder
       | Greq -> Llvm.build_icmp Llvm.Icmp.Sge l r "geq" builder
       | And -> Llvm.build_and l r "and" builder
       | Or -> Llvm.build_or l r "or" builder)
  | ExpIfElse (cond_expr, then_expr, else_expr) ->
    let* cond_val = codegen_expr env func cond_expr in
    let cond_bool = ensure_i1 cond_val in
    let then_bb = Llvm.append_block context "then" func in
    let else_bb = Llvm.append_block context "else" func in
    let merge_bb = Llvm.append_block context "merge" func in
    let _ = Llvm.build_cond_br cond_bool then_bb else_bb builder in
    Llvm.position_at_end then_bb builder;
    let* then_val = codegen_expr env func then_expr in
    let then_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end else_bb builder;
    let* else_val = codegen_expr env func else_expr in
    let else_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end merge_bb builder;
    Ok (Llvm.build_phi [ then_val, then_exit; else_val, else_exit ] "ifresult" builder)
  | ExpLetIn (Notrec, name, e1, e2) ->
    let* v = codegen_expr env func e1 in
    env_set env name v;
    codegen_expr env func e2
  | ExpApp _ as app ->
    let rec collect_call acc = function
      | ExpApp (f, arg, _) -> collect_call (arg :: acc) f
      | ExpVar (name, _) -> Ok (name, acc)
      | e -> Error (Printf.sprintf "Unsupported function expr: %s" (show_expression e))
    in
    let* fname, args = collect_call [] app in
    let* callee = lookup_func fname in
    let* arg_vals = map_result (codegen_expr env func) args in
    let n = List.length arg_vals in
    let ft = Llvm.function_type i64_t (Array.make n i64_t) in
    Ok (Llvm.build_call ft callee (Array.of_list arg_vals) "call" builder)
  | e -> Error (Printf.sprintf "Unsupported expression: %s" (show_expression e))

(* Unwrap nested ExpFun into (param_names, body) *)
and unwrap_fun = function
  | ExpFun (PatVar (name, _), body) ->
    let rest, b = unwrap_fun body in
    name :: rest, b
  | e -> [], e
;;

let codegen_func name body =
  let params, body_expr = unwrap_fun body in
  let n = List.length params in
  let ft = Llvm.function_type i64_t (Array.make n i64_t) in
  let fn =
    match Llvm.lookup_function name the_module with
    | Some f -> f
    | None -> Llvm.declare_function name ft the_module
  in
  let entry_bb = Llvm.append_block context "entry" fn in
  Llvm.position_at_end entry_bb builder;
  let env = new_env () in
  List.iteri
    (fun i pname ->
       let p = (Llvm.params fn).(i) in
       Llvm.set_value_name pname p;
       env_set env pname p)
    params;
  let* result = codegen_expr env fn body_expr in
  let _ = Llvm.build_ret result builder in
  Ok fn
;;

let codegen_main expr =
  let ft = Llvm.function_type i32_t [||] in
  let fn = Llvm.define_function "main" ft the_module in
  let entry_bb = Llvm.entry_block fn in
  Llvm.position_at_end entry_bb builder;
  let env = new_env () in
  let* result = codegen_expr env fn expr in
  let result_i32 = Llvm.build_trunc result i32_t "result" builder in
  let _ = Llvm.build_ret result_i32 builder in
  Ok fn
;;

let codegen_binding = function
  | Let (Rec, [ (PatVar (name, _), body) ]) ->
    let params, _ = unwrap_fun body in
    let n = List.length params in
    let ft = Llvm.function_type i64_t (Array.make n i64_t) in
    (match Llvm.lookup_function name the_module with
     | None ->
       let _ = Llvm.declare_function name ft the_module in
       ()
     | Some _ -> ());
    let* _ = codegen_func name body in
    Ok ()
  | Let (Notrec, [ (PatVar ("main", _), expr) ]) ->
    let* _ = codegen_main expr in
    Ok ()
  | Let (Notrec, [ (PatVar (name, _), body) ]) ->
    let* _ = codegen_func name body in
    Ok ()
  | Exp _ -> Ok ()
  | b -> Error (Printf.sprintf "Unsupported binding: %s" (show_bindings b))
;;

let codegen_program stmts output_file =
  let _ =
    Llvm.declare_function "print_int" (Llvm.function_type i64_t [| i64_t |]) the_module
  in
  let* () =
    List.fold_left
      (fun acc binding ->
         let* () = acc in
         codegen_binding binding)
      (Ok ())
      stmts
  in
  Ok (Llvm.print_module output_file the_module)
;;

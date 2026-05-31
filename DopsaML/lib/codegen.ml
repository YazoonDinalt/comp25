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
let i64v n = Llvm.const_int i64_t n
let tag_int n = i64v ((2 * n) + 1)
let untag v = Llvm.build_ashr v (i64v 1) "untag" builder

let retag v =
  Llvm.build_or (Llvm.build_shl v (i64v 1) "shl" builder) (i64v 1) "tag" builder
;;

let make_closure_ft = Llvm.function_type i64_t [| i64_t; i32_t |]
let apply_ft = Llvm.function_type i64_t [| i64_t; i64_t |]

module StringMap = Map.Make (String)

type arities = int StringMap.t
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

let build_make_closure fn_ptr arity =
  let* mc_fn = lookup_func "make_closure" in
  Ok
    (Llvm.build_call
       make_closure_ft
       mc_fn
       [| fn_ptr; Llvm.const_int i32_t arity |]
       "closure"
       builder)
;;

let build_apply closure_val arg_val =
  let* apply_fn = lookup_func "apply" in
  Ok (Llvm.build_call apply_ft apply_fn [| closure_val; arg_val |] "applied" builder)
;;

let apply_args init arg_vals =
  List.fold_left
    (fun acc arg ->
       let* c = acc in
       build_apply c arg)
    (Ok init)
    arg_vals
;;

let ensure_i1 v =
  if Llvm.type_of v = i1_t
  then v
  else Llvm.build_icmp Llvm.Icmp.Ne v (i64v 1) "tobool" builder
;;

(* Unwrap nested ExpFun into (param_names, body) *)
let rec unwrap_fun = function
  | ExpFun (PatVar (name, _), body) ->
    let rest, b = unwrap_fun body in
    name :: rest, b
  | e -> [], e
;;

let rec codegen_expr (arities : arities) (env : env) (func : Llvm.llvalue) = function
  | ExpConst (ConstInt n) -> Ok (tag_int n)
  | ExpConst (ConstBool b) -> Ok (i64v (if b then 3 else 1))
  | ExpVar (name, _) ->
    (match Hashtbl.find_opt env name with
     | Some v -> Ok v
     | None ->
       (match Llvm.lookup_function name the_module with
        | Some f ->
          let arity =
            match StringMap.find_opt name arities with
            | Some a -> a
            | None -> 1
          in
          let fn_ptr = Llvm.build_ptrtoint f i64_t "fptr" builder in
          build_make_closure fn_ptr arity
        | None -> Error (Printf.sprintf "Unbound variable: %s" name)))
  | ExpBinaryOp (op, e1, e2) ->
    let* l = codegen_expr arities env func e1 in
    let* r = codegen_expr arities env func e2 in
    let bool_op icmp_op =
      Llvm.build_select
        (Llvm.build_icmp icmp_op l r "cmp" builder)
        (i64v 3)
        (i64v 1)
        "bool"
        builder
    in
    Ok
      (match op with
       | Add -> Llvm.build_sub (Llvm.build_add l r "add" builder) (i64v 1) "tag" builder
       | Sub -> Llvm.build_add (Llvm.build_sub l r "sub" builder) (i64v 1) "tag" builder
       | Mul -> retag (Llvm.build_mul (untag l) (untag r) "mul" builder)
       | Div -> retag (Llvm.build_sdiv (untag l) (untag r) "div" builder)
       | Eq -> bool_op Llvm.Icmp.Eq
       | Neq -> bool_op Llvm.Icmp.Ne
       | Less -> bool_op Llvm.Icmp.Slt
       | Gre -> bool_op Llvm.Icmp.Sgt
       | Leq -> bool_op Llvm.Icmp.Sle
       | Greq -> bool_op Llvm.Icmp.Sge
       | And -> Llvm.build_and l r "and" builder
       | Or -> Llvm.build_or l r "or" builder)
  | ExpIfElse (cond_expr, then_expr, else_expr) ->
    let* cond_val = codegen_expr arities env func cond_expr in
    let cond_bool = ensure_i1 cond_val in
    let then_bb = Llvm.append_block context "then" func in
    let else_bb = Llvm.append_block context "else" func in
    let merge_bb = Llvm.append_block context "merge" func in
    let _ = Llvm.build_cond_br cond_bool then_bb else_bb builder in
    Llvm.position_at_end then_bb builder;
    let* then_val = codegen_expr arities env func then_expr in
    let then_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end else_bb builder;
    let* else_val = codegen_expr arities env func else_expr in
    let else_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end merge_bb builder;
    Ok (Llvm.build_phi [ then_val, then_exit; else_val, else_exit ] "ifresult" builder)
  | ExpLetIn (Notrec, name, e1, e2) ->
    let* v = codegen_expr arities env func e1 in
    env_set env name v;
    codegen_expr arities env func e2
  | ExpApp _ as app ->
    let rec collect_call acc = function
      | ExpApp (f, arg, _) -> collect_call (arg :: acc) f
      | ExpVar (name, _) -> Ok (name, acc)
      | e -> Error (Printf.sprintf "Unsupported function expr: %s" (show_expression e))
    in
    let* fname, args = collect_call [] app in
    let* arg_vals = map_result (codegen_expr arities env func) args in
    (match Hashtbl.find_opt env fname with
     | Some var_val -> apply_args var_val arg_vals
     | None ->
       (match Llvm.lookup_function fname the_module with
        | Some callee ->
          let arity =
            match StringMap.find_opt fname arities with
            | Some a -> a
            | None -> List.length arg_vals
          in
          let n = List.length arg_vals in
          if n = arity
          then (
            let ft = Llvm.function_type i64_t (Array.make n i64_t) in
            Ok (Llvm.build_call ft callee (Array.of_list arg_vals) "call" builder))
          else (
            let fn_ptr = Llvm.build_ptrtoint callee i64_t "fptr" builder in
            let* c0 = build_make_closure fn_ptr arity in
            apply_args c0 arg_vals)
        | None -> Error (Printf.sprintf "Unknown function: %s" fname)))
  | e -> Error (Printf.sprintf "Unsupported expression: %s" (show_expression e))
;;

let codegen_func arities name body =
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
  let* result = codegen_expr arities env fn body_expr in
  let _ = Llvm.build_ret result builder in
  Ok fn
;;

let codegen_main arities expr =
  let ft = Llvm.function_type i32_t [||] in
  let fn = Llvm.define_function "main" ft the_module in
  let entry_bb = Llvm.entry_block fn in
  Llvm.position_at_end entry_bb builder;
  let env = new_env () in
  let* result = codegen_expr arities env fn expr in
  let untagged = Llvm.build_ashr result (i64v 1) "untag" builder in
  let result_i32 = Llvm.build_trunc untagged i32_t "exitcode" builder in
  let _ = Llvm.build_ret result_i32 builder in
  Ok fn
;;

let codegen_binding arities = function
  | Let (Rec, [ (PatVar (name, _), body) ]) ->
    let params, _ = unwrap_fun body in
    let n = List.length params in
    let ft = Llvm.function_type i64_t (Array.make n i64_t) in
    (match Llvm.lookup_function name the_module with
     | None ->
       let _ = Llvm.declare_function name ft the_module in
       ()
     | Some _ -> ());
    let* _ = codegen_func arities name body in
    Ok ()
  | Let (Notrec, [ (PatVar ("main", _), expr) ]) ->
    let* _ = codegen_main arities expr in
    Ok ()
  | Let (Notrec, [ (PatVar (name, _), body) ]) ->
    let* _ = codegen_func arities name body in
    Ok ()
  | Exp _ -> Ok ()
  | b -> Error (Printf.sprintf "Unsupported binding: %s" (show_bindings b))
;;

let collect_arities stmts =
  List.fold_left
    (fun acc -> function
       | Let (_, pats) ->
         List.fold_left
           (fun acc (pat, expr) ->
              match pat with
              | PatVar (name, _) ->
                let params, _ = unwrap_fun expr in
                StringMap.add name (List.length params) acc
              | _ -> acc)
           acc
           pats
       | _ -> acc)
    StringMap.empty
    stmts
;;

let codegen_program stmts output_file =
  let _ =
    Llvm.declare_function "print_int" (Llvm.function_type i64_t [| i64_t |]) the_module
  in
  let _ = Llvm.declare_function "make_closure" make_closure_ft the_module in
  let _ = Llvm.declare_function "apply" apply_ft the_module in
  let arities = collect_arities stmts |> StringMap.add "print_int" 1 in
  let* () =
    List.fold_left
      (fun acc binding ->
         let* () = acc in
         codegen_binding arities binding)
      (Ok ())
      stmts
  in
  Ok (Llvm.print_module output_file the_module)
;;

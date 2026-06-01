(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf
module StringMap = Map.Make (String)

type arities = int StringMap.t

(* runtime functions the language can call, all int -> int *)
let builtins =
  [ "print_int"; "collect"; "print_gc_status"; "get_heap_start"; "get_heap_fin" ]
;;

let ( let* ) = Result.bind

let map_result f lst =
  List.fold_right
    (fun x acc ->
       let* acc = acc in
       let* v = f x in
       Ok (v :: acc))
    lst
    (Ok [])
;;

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

let void_t = Llvm.void_type context
let ptr_t = Llvm.pointer_type context
let make_closure_ft = Llvm.function_type i64_t [| i64_t; i32_t |]
let apply_ft = Llvm.function_type i64_t [| i64_t; i64_t |]
let builtin_ft = Llvm.function_type i64_t [| i64_t |]
let gc_init_ft = Llvm.function_type void_t [||]
let create_tuple_ft = Llvm.function_type i64_t [| i64_t; ptr_t |]
let field_ft = Llvm.function_type i64_t [| i64_t; i64_t |]

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

let build_apply fv av =
  let* apply_fn = lookup_func "apply" in
  Ok (Llvm.build_call apply_ft apply_fn [| fv; av |] "applied" builder)
;;

let ensure_i1 v =
  if Llvm.type_of v = i1_t
  then v
  else Llvm.build_icmp Llvm.Icmp.Ne v (i64v 1) "tobool" builder
;;

let build_closure_of fname arity =
  let* f = lookup_func fname in
  let fn_ptr = Llvm.build_ptrtoint f i64_t "fptr" builder in
  build_make_closure fn_ptr arity
;;

let arity_of arities name =
  match StringMap.find_opt name arities with
  | Some a -> a
  | None -> 1
;;

let codegen_imm arities (env : env) = function
  | ImmInt n -> Ok (tag_int n)
  | ImmBool b -> Ok (i64v (if b then 3 else 1))
  | ImmUnit -> Ok (i64v 1)
  | ImmVar x when Hashtbl.mem env x -> env_get env x
  (* a top-level function used as a value -> closure *)
  | ImmVar x -> build_closure_of x (arity_of arities x)
;;

(* apply [args] to [start] one at a time through the runtime *)
let apply_chain start args =
  List.fold_left
    (fun acc av ->
       let* fv = acc in
       build_apply fv av)
    start
    args
;;

let rec codegen_cexpr arities (env : env) (func : Llvm.llvalue) = function
  | CImm i -> codegen_imm arities env i
  | CBinop (op, le, re) ->
    let* l = codegen_imm arities env le in
    let* r = codegen_imm arities env re in
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
  | CApp (f, args) ->
    let* argvals = map_result (codegen_imm arities env) args in
    if Hashtbl.mem env f
    then apply_chain (env_get env f) argvals (* local closure *)
    else (
      let arity = arity_of arities f in
      if List.length args = arity
      then
        (* exact arity: direct call *)
        let* callee = lookup_func f in
        let ft = Llvm.function_type i64_t (Array.make arity i64_t) in
        Ok (Llvm.build_call ft callee (Array.of_list argvals) "call" builder)
      else apply_chain (build_closure_of f arity) argvals)
  | CTuple fields ->
    let* vals = map_result (codegen_imm arities env) fields in
    let n = List.length vals in
    let arr = Llvm.build_array_alloca i64_t (i64v n) "tuple_fields" builder in
    List.iteri
      (fun i v ->
         let gep = Llvm.build_gep i64_t arr [| i64v i |] "fp" builder in
         let (_ : Llvm.llvalue) = Llvm.build_store v gep builder in
         ())
      vals;
    let* ct = lookup_func "create_tuple" in
    Ok (Llvm.build_call create_tuple_ft ct [| i64v n; arr |] "tuple" builder)
  | CField (v, i) ->
    let* tv = codegen_imm arities env v in
    let* fld = lookup_func "field" in
    Ok (Llvm.build_call field_ft fld [| tv; tag_int i |] "field" builder)
  | CIf (cond, then_a, else_a) ->
    let* cond_val = codegen_imm arities env cond in
    let cond_bool = ensure_i1 cond_val in
    let then_bb = Llvm.append_block context "then" func in
    let else_bb = Llvm.append_block context "else" func in
    let merge_bb = Llvm.append_block context "merge" func in
    let _ = Llvm.build_cond_br cond_bool then_bb else_bb builder in
    Llvm.position_at_end then_bb builder;
    let* then_val = codegen_aexpr arities env func then_a in
    let then_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end else_bb builder;
    let* else_val = codegen_aexpr arities env func else_a in
    let else_exit = Llvm.insertion_block builder in
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end merge_bb builder;
    Ok (Llvm.build_phi [ then_val, then_exit; else_val, else_exit ] "ifresult" builder)

and codegen_aexpr arities (env : env) (func : Llvm.llvalue) = function
  | ACExpr c -> codegen_cexpr arities env func c
  | ALet (name, c, rest) ->
    let* v = codegen_cexpr arities env func c in
    env_set env name v;
    codegen_aexpr arities env func rest
;;

let collect_arities (prog : aprogram) =
  List.fold_left
    (fun m (f : afunc) -> StringMap.add f.name (List.length f.params) m)
    (List.fold_left (fun m name -> StringMap.add name 1 m) StringMap.empty builtins)
    prog
;;

let codegen_func arities (f : afunc) =
  let n = List.length f.params in
  let ft = Llvm.function_type i64_t (Array.make n i64_t) in
  let fn =
    match Llvm.lookup_function f.name the_module with
    | Some x -> x
    | None -> Llvm.declare_function f.name ft the_module
  in
  let entry_bb = Llvm.append_block context "entry" fn in
  Llvm.position_at_end entry_bb builder;
  let env = new_env () in
  List.iteri
    (fun i pname ->
       let p = (Llvm.params fn).(i) in
       Llvm.set_value_name pname p;
       env_set env pname p)
    f.params;
  let* result = codegen_aexpr arities env fn f.body in
  let _ = Llvm.build_ret result builder in
  Ok fn
;;

let codegen_main arities (f : afunc) =
  let ft = Llvm.function_type i32_t [||] in
  let fn = Llvm.define_function "main" ft the_module in
  let entry_bb = Llvm.entry_block fn in
  Llvm.position_at_end entry_bb builder;
  let* gc_init_fn = lookup_func "gc_init" in
  let _ = Llvm.build_call gc_init_ft gc_init_fn [||] "" builder in
  let env = new_env () in
  let* result = codegen_aexpr arities env fn f.body in
  let untagged = Llvm.build_ashr result (i64v 1) "untag" builder in
  let result_i32 = Llvm.build_trunc untagged i32_t "exitcode" builder in
  let _ = Llvm.build_ret result_i32 builder in
  Ok fn
;;

let codegen_cfunc arities (f : afunc) =
  if f.name = "main"
  then
    let* _ = codegen_main arities f in
    Ok ()
  else if f.is_rec
  then (
    let n = List.length f.params in
    let ft = Llvm.function_type i64_t (Array.make n i64_t) in
    match Llvm.lookup_function f.name the_module with
    | None ->
      let _ = Llvm.declare_function f.name ft the_module in
      let* _ = codegen_func arities f in
      Ok ()
    | Some _ ->
      let* _ = codegen_func arities f in
      Ok ())
  else
    let* _ = codegen_func arities f in
    Ok ()
;;

let codegen_program (prog : aprogram) output_file =
  let arities = collect_arities prog in
  List.iter
    (fun name ->
       let (_ : Llvm.llvalue) = Llvm.declare_function name builtin_ft the_module in
       ())
    builtins;
  let _ = Llvm.declare_function "make_closure" make_closure_ft the_module in
  let _ = Llvm.declare_function "apply" apply_ft the_module in
  let _ = Llvm.declare_function "gc_init" gc_init_ft the_module in
  let _ = Llvm.declare_function "create_tuple" create_tuple_ft the_module in
  let _ = Llvm.declare_function "field" field_ft the_module in
  List.iter
    (fun (f : afunc) ->
       if f.name <> "main"
       then (
         let n = List.length f.params in
         let ft = Llvm.function_type i64_t (Array.make n i64_t) in
         match Llvm.lookup_function f.name the_module with
         | None ->
           let _ = Llvm.declare_function f.name ft the_module in
           ()
         | Some _ -> ()))
    prog;
  let* () =
    List.fold_left
      (fun acc f ->
         let* () = acc in
         codegen_cfunc arities f)
      (Ok ())
      prog
  in
  Ok (Llvm.print_module output_file the_module)
;;

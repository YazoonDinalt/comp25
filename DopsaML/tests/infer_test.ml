(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib
open Inferencer
open Ast
open Ty

let pretty_op_name = function
  | "Add" -> "(+)"
  | "Sub" -> "(-)" 
  | "Mul" -> "(*)"
  | "Div" -> "(/)"
  | "And" -> "(&&)"
  | "Or" -> "(||)"
  | "Eq" -> "(=)"
  | "Neq" -> "(<>)"
  | "Less" -> "(<)"
  | "Gre" -> "(>)"
  | "Leq" -> "(<=)"
  | "Greq" -> "(>=)"
  | name -> name


let pretty_printer_parse_and_infer s =
  match Parser.parser s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         let pretty_key = pretty_op_name key in
         Format.printf "val %s: %a\n" pretty_key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error_infer e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let pretty_printer_parse_and_infer_simple s =
  match Parser.parser s with
  | Ok parsed ->
    (match parsed with
     | [ Exp expr ] ->
       (match infer_simple_expression expr with
        | Ok ty -> Format.printf "%a\n" pp_ty ty
        | Error e -> Format.printf "Infer error. %a\n" pp_error_infer e)
     | _ ->
       Format.printf
         "Expected a single expression, but got a program with multiple structures.\n")
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer_simple "10/2 + 56*2 - 10 / 10 / 20 + 666 - 777 + 1";
  [%expect
    {|
    int|}]
;;

let%expect_test "test_bool" =
  pretty_printer_parse_and_infer_simple "false";
  [%expect {|bool|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pretty_printer_parse_and_infer_simple "fun x -> x * 69 + 100 - 201 / 777";
  [%expect
    {|
    int -> int|}]
;;

let%expect_test "test_rec" =
  pretty_printer_parse_and_infer "let rec func arg = func arg";
  [%expect
    {|
    val func: 'a -> 'b|}]
;;

let%expect_test "test_func_apply_some_args" =
  pretty_printer_parse_and_infer "let func a1 a2 a3 = a1 a2 a3";
  [%expect
    {|
    val func: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c|}]
;;

let%expect_test "test_list" =
  pretty_printer_parse_and_infer "let arr = [1;2;3]";
  [%expect {|
    val arr: int list |}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = if x > 10 then true else false ";
  [%expect
    {|
    val is_above_10: int -> bool|}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = x > 10";
  [%expect
    {|
    val is_above_10: int -> bool|}]
;;

let%expect_test "test_factorial" =
  pretty_printer_parse_and_infer "let rec fac n = if n < 2 then 1 else n * fac (n - 1)";
  [%expect
    {|
    val fac: int -> int|}]
;;

let%expect_test "test_nested_list_function" =
  pretty_printer_parse_and_infer "let f x = [ [x; x]; [x] ]";
  [%expect {|
    val f: 'a -> 'a list list |}]
;;


let%expect_test "test_fibonacci" =
  pretty_printer_parse_and_infer
    "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2)";
  [%expect
    {|
    val fibo: int -> int|}]
;;

let%expect_test "test_unbound_var" =
  pretty_printer_parse_and_infer "let f = x";
  [%expect {|Infer error. Typechecker error: undefined variable 'x'|}]
;;

let%expect_test "test_annotate" =
  pretty_printer_parse_and_infer "let sum = fun (x : int) (y : int) -> x + y";
  [%expect
    {|
    val sum: int -> int -> int|}]
;;

let%expect_test "test_annotate_fac" =
  pretty_printer_parse_and_infer
    "let rec fac = fun (n : int) (acc : int) -> if n < 2 then acc else fac (n-1) (acc * \
     n);;";
  [%expect
    {|
    val fac: int -> int -> int|}]
;;

let%expect_test "test_redefinition1" =
  pretty_printer_parse_and_infer "let (+) = fun (x: int) (y: int) -> x - y";
  [%expect
    {|
      val (+): int -> int -> int |}]
;;

let%expect_test "test_redefinition" =
  pretty_printer_parse_and_infer "let (+) a b = a && b";
  [%expect
    {|
      val (+): bool -> bool -> bool |}]
;;

let%expect_test "test_redefinition" =
  pretty_printer_parse_and_infer "let (+) a b = if a > b then false else true";
  [%expect
    {|
      val (+): 'a -> 'a -> bool |}]
;;

let%expect_test "test_program_1" =
  pretty_printer_parse_and_infer
    "let div = fun x y -> x / y \n\
    \     let sum = fun x y -> x + y\n\
    \     let res = fun x y z -> div x (sum y z)";
  [%expect
    {|
    val div: int -> int -> int
    val res: int -> int -> int -> int
    val sum: int -> int -> int|}]
;;

let%expect_test "test_program_2" =
  pretty_printer_parse_and_infer
    "let square = fun x -> x * x\n\
    \                                  let result = square 10";
  [%expect
    {|
    val result: int
    val square: int -> int|}]
;;

let%expect_test "test_annotate_error" =
  pretty_printer_parse_and_infer "let sum (x: int) (y: bool) = x + y";
  [%expect
    {|
    Infer error. Typechecker error: unification failed on bool and int|}]
;;

let%expect_test "test_unification_types" =
  pretty_printer_parse_and_infer "fun x -> x + true";
  [%expect
    {|
    Infer error. Typechecker error: unification failed on bool and int|}]
;;

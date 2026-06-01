(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib

(* run the whole pipeline and report the outcome as a string *)
let compile src =
  match Parser.parser src with
  | Error e -> Printf.sprintf "parse error: %s" e
  | Ok stmts ->
    (match Anf.anf_program (Ll.ll_program (Cc.cc_program stmts)) with
     | Error e -> Printf.sprintf "anf error: %s" e
     | Ok prog ->
       (match Codegen.codegen_program prog "/dev/null" with
        | Ok () -> "ok"
        | Error e -> Printf.sprintf "codegen error: %s" e))
;;

let%expect_test "pipeline covers the backend" =
  let p s = print_endline (compile s) in
  p
    {|let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
let main = print_int (fac 5)|};
  p
    {|let ops a b =
  let s = a + b - a * b / b in
  let c = if a = b then a <> b else a < b in
  let d = if a > b then a <= b else a >= b in
  let e = if a && b then a || b else b in
  s + c + d + e
let main = print_int (ops 6 2)|};
  p
    {|let add a b c = a + b + c
let main =
  let f = add 1 in
  let g = f 2 in
  print_int (g 3)|};
  p
    {|let app f x = f x
let inc x = x + 1
let main = print_int (app inc 10)|};
  p
    {|let main =
  let t = (1, (2, (fun x -> x + 1))) in
  let (a, (b, f)) = t in
  print_int (f (a + b))|};
  p
    {|let main =
  let _ = print_gc_status () in
  let _ = collect () in
  print_int (get_heap_fin () - get_heap_start ())|};
  p
    {|let f x = if true then x else false
let main = print_int (f 7)|};
  p
    {|let f a =
  let g = fun x -> x + a in
  g 5
let main = print_int (f 10)|};
  p
    {|let main =
  let (_, b) = (1, 2) in
  print_int b|};
  p {|let main = print_int unknownvar|};
  p {|let main = [ 1; 2; 3 ]|};
  [%expect
    {|
    ok
    ok
    ok
    ok
    ok
    ok
    ok
    ok
    ok
    codegen error: Unknown function: unknownvar
    anf error: ANF: unsupported expression |}]
;;

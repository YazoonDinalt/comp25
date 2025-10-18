(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib
open Ast
open Parser

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test pattern parser *)

let%expect_test "pattern test" =
  let test = "true" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PatConst (ConstBool true)) |}]
;;

let%expect_test "pattern test" =
  let test = "951753" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PatConst (ConstInt 951753)) |}]
;;

let%expect_test "pattern test" =
  let test = "var" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PatVar ("var", TypeUnknown)) |}]
;;

let%expect_test "pattern test" =
  let test = "(var : int)" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PatVar ("var", TypeInt)) |}]
;;

let%expect_test "pattern test" =
  let test = "((1, 2), (a: int), true)" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (PatTuple
       [(PatTuple [(PatConst (ConstInt 1)); (PatConst (ConstInt 2))]);
         (PatVar ("a", TypeInt)); (PatConst (ConstBool true))]) |}]
;;

let%expect_test "pattern test" =
  let test = "[1 :: (a: int); [1; 2]; (1, 2, 3); true]" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (PatCon ((PatCon ((PatConst (ConstInt 1)), (PatVar ("a", TypeInt)))),
       (PatCon ((PatCon ((PatConst (ConstInt 1)), (PatConst (ConstInt 2)))),
          (PatCon (
             (PatTuple
                [(PatConst (ConstInt 1)); (PatConst (ConstInt 2));
                  (PatConst (ConstInt 3))]),
             (PatConst (ConstBool true))))
          ))
       )) |}]
;;

(* Test expression *)

(* EConst *)

let%expect_test "expression_test" =
  let test = "1" in
  start_test parse_Exp show_expression test;
  [%expect {| (ExpConst (ConstInt 1)) |}]
;;

let%expect_test "expression_test" =
  let test = "false" in
  start_test parse_Exp show_expression test;
  [%expect {| (ExpConst (ConstBool false)) |}]
;;

(* EVar *)

let%expect_test "expression_test" =
  let test = "papa" in
  start_test parse_Exp show_expression test;
  [%expect {| (ExpVar ("papa", TypeUnknown)) |}]
;;

let%expect_test "expression_test" =
  let test = "(papa : int)" in
  start_test parse_Exp show_expression test;
  [%expect {| (ExpVar ("papa", TypeInt)) |}]
;;

let%expect_test "expression_test" =
  let test = "([1; ([2; ([3; 4], 5)], 6)], 7)" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpTuple
       [(ExpList ((ExpConst (ConstInt 1)),
           (ExpList (
              (ExpTuple
                 [(ExpList ((ExpConst (ConstInt 2)),
                     (ExpList (
                        (ExpTuple
                           [(ExpList ((ExpConst (ConstInt 3)),
                               (ExpList ((ExpConst (ConstInt 4)),
                                  (ExpConst ConstNil)))
                               ));
                             (ExpConst (ConstInt 5))]),
                        (ExpConst ConstNil)))
                     ));
                   (ExpConst (ConstInt 6))]),
              (ExpConst ConstNil)))
           ));
         (ExpConst (ConstInt 7))]) |}]
;;

let%expect_test "expression_test" =
  let test = "[1; 5]" in
  start_test parse_Exp show_expression test;
  [%expect {|
    (ExpList ((ExpConst (ConstInt 1)),
       (ExpList ((ExpConst (ConstInt 5)), (ExpConst ConstNil))))) |}]
;;

(* EBinaryOp *)

let%expect_test "expression_test" =
  let test = "1 + (a * 3) - x" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpBinaryOp (Sub,
       (ExpBinaryOp (Add, (ExpConst (ConstInt 1)),
          (ExpBinaryOp (Mul, (ExpVar ("a", TypeUnknown)), (ExpConst (ConstInt 3))
             ))
          )),
       (ExpVar ("x", TypeUnknown)))) |}]
;;

let%expect_test "expression_test" =
  let test = "true || false" in
  start_test parse_Exp show_expression test;
  [%expect {| (ExpBinaryOp (Or, (ExpConst (ConstBool true)), (ExpConst (ConstBool false)))) |}]
;;

(* EIfElse *)

let%expect_test "expression_test" =
  let test = "if n = 1 then 1 else 3" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpIfElse (
       (ExpBinaryOp (Eq, (ExpVar ("n", TypeUnknown)), (ExpConst (ConstInt 1)))),
       (ExpConst (ConstInt 1)), (ExpConst (ConstInt 3)))) |}]
;;

(* EFun *)

let%expect_test "expression_test" =
  let test = "fun x -> 3 + x" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpFun ((PatVar ("x", TypeUnknown)),
       (ExpBinaryOp (Add, (ExpConst (ConstInt 3)), (ExpVar ("x", TypeUnknown))))
       )) |}]
;;

let%expect_test "expression_test" =
  let test = "fun x y -> y + x" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpFun ((PatVar ("x", TypeUnknown)),
       (ExpFun ((PatVar ("y", TypeUnknown)),
          (ExpBinaryOp (Add, (ExpVar ("y", TypeUnknown)),
             (ExpVar ("x", TypeUnknown))))
          ))
       )) |}]
;;

(* EApp *)

let%expect_test "expression_test" =
  let test = "is_something yes no" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpApp (
       (ExpApp ((ExpVar ("is_something", TypeUnknown)),
          (ExpVar ("yes", TypeUnknown)), TypeUnknown)),
       (ExpVar ("no", TypeUnknown)), TypeUnknown)) |}]
;;

let%expect_test "expression_test" =
  let test = "(fun x -> x + 1) 1" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpApp (
       (ExpFun ((PatVar ("x", TypeUnknown)),
          (ExpBinaryOp (Add, (ExpVar ("x", TypeUnknown)), (ExpConst (ConstInt 1))
             ))
          )),
       (ExpConst (ConstInt 1)), TypeUnknown)) |}]
;;

(* ELetIn *)

let%expect_test "expression_test" =
  let test = "let sum a b = a + b in sum 2 3" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpLetIn (Notrec, "sum",
       (ExpFun ((PatVar ("a", TypeUnknown)),
          (ExpFun ((PatVar ("b", TypeUnknown)),
             (ExpBinaryOp (Add, (ExpVar ("a", TypeUnknown)),
                (ExpVar ("b", TypeUnknown))))
             ))
          )),
       (ExpApp (
          (ExpApp ((ExpVar ("sum", TypeUnknown)), (ExpConst (ConstInt 2)),
             TypeUnknown)),
          (ExpConst (ConstInt 3)), TypeUnknown))
       )) |}]
;;

(**  List and tuple *)

let%expect_test "expression_test" =
  let test = "[1;2;3;4]" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpList ((ExpConst (ConstInt 1)),
       (ExpList ((ExpConst (ConstInt 2)),
          (ExpList ((ExpConst (ConstInt 3)),
             (ExpList ((ExpConst (ConstInt 4)), (ExpConst ConstNil)))))
          ))
       ))
 |}]
;;

let%expect_test "expression_test" =
  let test = "(1,2,3,4,5)" in
  start_test parse_Exp show_expression test;
  [%expect
    {|
    (ExpTuple
       [(ExpConst (ConstInt 1)); (ExpConst (ConstInt 2));
         (ExpConst (ConstInt 3)); (ExpConst (ConstInt 4));
         (ExpConst (ConstInt 5))])
 |}]
;;

(** Test bindings *)

let%expect_test "bindings_test" =
  let test = "let plusfive x = let five a = a + 5 in five x" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("plusfive", TypeUnknown)),
         (ExpFun ((PatVar ("x", TypeUnknown)),
            (ExpLetIn (Notrec, "five",
               (ExpFun ((PatVar ("a", TypeUnknown)),
                  (ExpBinaryOp (Add, (ExpVar ("a", TypeUnknown)),
                     (ExpConst (ConstInt 5))))
                  )),
               (ExpApp ((ExpVar ("five", TypeUnknown)),
                  (ExpVar ("x", TypeUnknown)), TypeUnknown))
               ))
            )))
         ]
       )) |}]
;;

let%expect_test "bindings_test" =
  let test = "4 + 3" in
  start_test parse_bindings show_bindings test;
  [%expect {|
    (Exp (ExpBinaryOp (Add, (ExpConst (ConstInt 4)), (ExpConst (ConstInt 3))))) |}]
;;

let%expect_test "bindings_test" =
  let test = "let (+) x y = x - y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("Add", (TypeArrow (TypeInt, (TypeArrow (TypeInt, TypeInt)))))),
         (ExpFun ((PatVar ("x", TypeUnknown)),
            (ExpFun ((PatVar ("y", TypeUnknown)),
               (ExpBinaryOp (Sub, (ExpVar ("x", TypeUnknown)),
                  (ExpVar ("y", TypeUnknown))))
               ))
            )))
         ]
       ))|}]
;;

let%expect_test "bindings_test" =
  let test = "let f (x: int) (y: int) = x + y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("f", TypeUnknown)),
         (ExpFun ((PatVar ("x", TypeInt)),
            (ExpFun ((PatVar ("y", TypeInt)),
               (ExpBinaryOp (Add, (ExpVar ("x", TypeUnknown)),
                  (ExpVar ("y", TypeUnknown))))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "[1;2;3;4]" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Exp
       (ExpList ((ExpConst (ConstInt 1)),
          (ExpList ((ExpConst (ConstInt 2)),
             (ExpList ((ExpConst (ConstInt 3)),
                (ExpList ((ExpConst (ConstInt 4)), (ExpConst ConstNil)))))
             ))
          )))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "(1,2,3,4,5)" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Exp
       (ExpTuple
          [(ExpConst (ConstInt 1)); (ExpConst (ConstInt 2));
            (ExpConst (ConstInt 3)); (ExpConst (ConstInt 4));
            (ExpConst (ConstInt 5))]))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let f (x: int) = x + 4" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("f", TypeUnknown)),
         (ExpFun ((PatVar ("x", TypeInt)),
            (ExpBinaryOp (Add, (ExpVar ("x", TypeUnknown)),
               (ExpConst (ConstInt 4))))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "2 * (4 + 4) * 1" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Exp
       (ExpBinaryOp (Mul,
          (ExpBinaryOp (Mul, (ExpConst (ConstInt 2)),
             (ExpBinaryOp (Add, (ExpConst (ConstInt 4)), (ExpConst (ConstInt 4))
                ))
             )),
          (ExpConst (ConstInt 1)))))
 |}]
;;

let%expect_test "bindings_test" =
  let test =
    "let rec fib n = \n\
    \      if n < 1 \n\
    \        then 1 \n\
    \      else fib (n - 1) + fib (n - 2)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec,
       [((PatVar ("fib", TypeUnknown)),
         (ExpFun ((PatVar ("n", TypeUnknown)),
            (ExpIfElse (
               (ExpBinaryOp (Less, (ExpVar ("n", TypeUnknown)),
                  (ExpConst (ConstInt 1)))),
               (ExpConst (ConstInt 1)),
               (ExpBinaryOp (Add,
                  (ExpApp ((ExpVar ("fib", TypeUnknown)),
                     (ExpBinaryOp (Sub, (ExpVar ("n", TypeUnknown)),
                        (ExpConst (ConstInt 1)))),
                     TypeUnknown)),
                  (ExpApp ((ExpVar ("fib", TypeUnknown)),
                     (ExpBinaryOp (Sub, (ExpVar ("n", TypeUnknown)),
                        (ExpConst (ConstInt 2)))),
                     TypeUnknown))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test =
    "let rec cps_fact x = \n\
    \      let helper x acc = \n\
    \        if x = 1 \n\
    \          then acc x \n\
    \        else helper (x - 1) (fun n -> n * acc x) \n\
    \      in \n\
    \      helper x (fun a -> a)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec,
       [((PatVar ("cps_fact", TypeUnknown)),
         (ExpFun ((PatVar ("x", TypeUnknown)),
            (ExpLetIn (Notrec, "helper",
               (ExpFun ((PatVar ("x", TypeUnknown)),
                  (ExpFun ((PatVar ("acc", TypeUnknown)),
                     (ExpIfElse (
                        (ExpBinaryOp (Eq, (ExpVar ("x", TypeUnknown)),
                           (ExpConst (ConstInt 1)))),
                        (ExpApp ((ExpVar ("acc", TypeUnknown)),
                           (ExpVar ("x", TypeUnknown)), TypeUnknown)),
                        (ExpApp (
                           (ExpApp ((ExpVar ("helper", TypeUnknown)),
                              (ExpBinaryOp (Sub, (ExpVar ("x", TypeUnknown)),
                                 (ExpConst (ConstInt 1)))),
                              TypeUnknown)),
                           (ExpFun ((PatVar ("n", TypeUnknown)),
                              (ExpBinaryOp (Mul, (ExpVar ("n", TypeUnknown)),
                                 (ExpApp ((ExpVar ("acc", TypeUnknown)),
                                    (ExpVar ("x", TypeUnknown)), TypeUnknown))
                                 ))
                              )),
                           TypeUnknown))
                        ))
                     ))
                  )),
               (ExpApp (
                  (ExpApp ((ExpVar ("helper", TypeUnknown)),
                     (ExpVar ("x", TypeUnknown)), TypeUnknown)),
                  (ExpFun ((PatVar ("a", TypeUnknown)),
                     (ExpVar ("a", TypeUnknown)))),
                  TypeUnknown))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let f = fun (g: int -> int -> int) (x: int) (y: int) -> g x y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("f", TypeUnknown)),
         (ExpFun (
            (PatVar ("g", (TypeArrow (TypeInt, (TypeArrow (TypeInt, TypeInt)))))),
            (ExpFun ((PatVar ("x", TypeInt)),
               (ExpFun ((PatVar ("y", TypeInt)),
                  (ExpApp (
                     (ExpApp ((ExpVar ("g", TypeUnknown)),
                        (ExpVar ("x", TypeUnknown)), TypeUnknown)),
                     (ExpVar ("y", TypeUnknown)), TypeUnknown))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let (+) = fun (x: int) (y: int) -> x - y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("Add", (TypeArrow (TypeInt, (TypeArrow (TypeInt, TypeInt)))))),
         (ExpFun ((PatVar ("x", TypeInt)),
            (ExpFun ((PatVar ("y", TypeInt)),
               (ExpBinaryOp (Sub, (ExpVar ("x", TypeUnknown)),
                  (ExpVar ("y", TypeUnknown))))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let h f1 f2 f3 f4 f5 f6 f7 = f1 (f2 (f3 (f4 (f5 (f6 f7)))))" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("h", TypeUnknown)),
         (ExpFun ((PatVar ("f1", TypeUnknown)),
            (ExpFun ((PatVar ("f2", TypeUnknown)),
               (ExpFun ((PatVar ("f3", TypeUnknown)),
                  (ExpFun ((PatVar ("f4", TypeUnknown)),
                     (ExpFun ((PatVar ("f5", TypeUnknown)),
                        (ExpFun ((PatVar ("f6", TypeUnknown)),
                           (ExpFun ((PatVar ("f7", TypeUnknown)),
                              (ExpApp ((ExpVar ("f1", TypeUnknown)),
                                 (ExpApp ((ExpVar ("f2", TypeUnknown)),
                                    (ExpApp ((ExpVar ("f3", TypeUnknown)),
                                       (ExpApp ((ExpVar ("f4", TypeUnknown)),
                                          (ExpApp ((ExpVar ("f5", TypeUnknown)),
                                             (ExpApp (
                                                (ExpVar ("f6", TypeUnknown)),
                                                (ExpVar ("f7", TypeUnknown)),
                                                TypeUnknown)),
                                             TypeUnknown)),
                                          TypeUnknown)),
                                       TypeUnknown)),
                                    TypeUnknown)),
                                 TypeUnknown))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let h f1 f2 f3 f4 f5 f6 f7 = ((((((f1 f2) f3) f4) f5) f6) f7)" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PatVar ("h", TypeUnknown)),
         (ExpFun ((PatVar ("f1", TypeUnknown)),
            (ExpFun ((PatVar ("f2", TypeUnknown)),
               (ExpFun ((PatVar ("f3", TypeUnknown)),
                  (ExpFun ((PatVar ("f4", TypeUnknown)),
                     (ExpFun ((PatVar ("f5", TypeUnknown)),
                        (ExpFun ((PatVar ("f6", TypeUnknown)),
                           (ExpFun ((PatVar ("f7", TypeUnknown)),
                              (ExpApp (
                                 (ExpApp (
                                    (ExpApp (
                                       (ExpApp (
                                          (ExpApp (
                                             (ExpApp (
                                                (ExpVar ("f1", TypeUnknown)),
                                                (ExpVar ("f2", TypeUnknown)),
                                                TypeUnknown)),
                                             (ExpVar ("f3", TypeUnknown)),
                                             TypeUnknown)),
                                          (ExpVar ("f4", TypeUnknown)),
                                          TypeUnknown)),
                                       (ExpVar ("f5", TypeUnknown)), TypeUnknown
                                       )),
                                    (ExpVar ("f6", TypeUnknown)), TypeUnknown)),
                                 (ExpVar ("f7", TypeUnknown)), TypeUnknown))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;
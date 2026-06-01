(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib

let anf src =
  match Parser.parser src with
  | Error e -> print_endline e
  | Ok stmts ->
    (match Anf.anf_program (Ll.ll_program (Cc.cc_program stmts)) with
     | Error e -> print_endline e
     | Ok prog -> print_endline (Anf.show_aprogram prog))
;;

let%expect_test "nested arithmetic is flattened into lets" =
  anf {|let main = print_int (1 + 2 * 3)|};
  [%expect
    {|
    [{ name = "main"; is_rec = false; params = [];
       body =
       (ALet ("anf1", (CBinop (Mul, (ImmInt 2), (ImmInt 3))),
          (ALet ("anf0", (CBinop (Add, (ImmInt 1), (ImmVar "anf1"))),
             (ACExpr (CApp ("print_int", [(ImmVar "anf0")])))))
          ))
       }
      ] |}]
;;

let%expect_test "if and tuple destructuring" =
  anf {|let main = let (a, b) = (1, 2) in if a then a else b|};
  [%expect
    {|
    [{ name = "main"; is_rec = false; params = [];
       body =
       (ALet ("anf0", (CTuple [(ImmInt 1); (ImmInt 2)]),
          (ALet ("a", (CField ((ImmVar "anf0"), 0)),
             (ALet ("b", (CField ((ImmVar "anf0"), 1)),
                (ACExpr
                   (CIf ((ImmVar "a"), (ACExpr (CImm (ImmVar "a"))),
                      (ACExpr (CImm (ImmVar "b"))))))
                ))
             ))
          ))
       }
      ] |}]
;;

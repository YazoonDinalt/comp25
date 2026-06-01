(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib

let cc src =
  match Parser.parser src with
  | Error e -> print_endline e
  | Ok stmts -> print_endline (Ast.show_statements (Cc.cc_program stmts))
;;

let%expect_test "free variable becomes a leading parameter" =
  cc {|let f a = let g = fun x -> x + a in g 5|};
  [%expect
    {|
    [(Let (Notrec,
        [((PatVar ("f", TypeUnknown)),
          (ExpFun ((PatVar ("a", TypeUnknown)),
             (ExpLetIn (Notrec, "g",
                (ExpApp (
                   (ExpFun ((PatVar ("a", TypeUnknown)),
                      (ExpFun ((PatVar ("x", TypeUnknown)),
                         (ExpBinaryOp (Add, (ExpVar ("x", TypeUnknown)),
                            (ExpVar ("a", TypeUnknown))))
                         ))
                      )),
                   (ExpVar ("a", TypeUnknown)), TypeUnknown)),
                (ExpApp ((ExpVar ("g", TypeUnknown)), (ExpConst (ConstInt 5)),
                   TypeUnknown))
                ))
             )))
          ]
        ))
      ] |}]
;;

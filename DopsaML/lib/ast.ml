(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | ConstInt of int (** integer number: ..., 0, 1, 2, ...*)
  | ConstBool of bool (** boolean values: true and false *)
  | ConstNil (** [] *)
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Add (** 1 + 2 *)
  | Sub (** 2 - 1 *)
  | Mul (** * *)
  | Div (** / *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving eq, show { with_path = false }]

type type_of_var =
  | TypeInt (** int type for variable *)
  | TypeBool (** bool type for variable *)
  | TypeUnknown (** unknown type for variable *)
  | TypeArrow of type_of_var * type_of_var (** type int -> int... *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PatWild (** _ *)
  | PatConst of const (** constant pattern *)
  | PatVar of name * type_of_var (** variable pattern*)
  | PatTuple of pattern list (** (a, b) *)
  | PatCon of pattern * pattern (** hd :: tl *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec (** let rec v = ... *)
  | Notrec (** let z = ...*)
[@@deriving eq, show { with_path = false }]

type expression =
  | ExpConst of const (** constant *)
  | ExpVar of name * type_of_var (** variable *)
  | ExpBinaryOp of binary_op * expression * expression (** binary operation *)
  | ExpApp of expression * expression * type_of_var (** application *)
  | ExpIfElse of expression * expression * expression (** if z then v else n*)
  | ExpLetIn of rec_flag * name * expression * expression
  | ExpLetPatIn of pattern * expression * expression
  | ExpFun of pattern * expression (** fun z -> z + z *)
  | ExpList of expression * expression (** [1;2;3]*)
  | ExpTuple of expression list (** (1,2,3) *)
[@@deriving eq, show { with_path = false }]

(** Binding type *)
type bindings =
  | Let of rec_flag * (pattern * expression) list
  | Exp of expression (** simple expressions *)
[@@deriving eq, show { with_path = false }]

(** Statements type *)
type statements = bindings list [@@deriving eq, show { with_path = false }]
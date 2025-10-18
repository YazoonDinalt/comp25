(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(** Start parse func *)

let start_parsing parser string = parse_string ~consume:All parser string

(* Base *)

let is_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_bchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "val"
  | "in"
  | "and" -> true
  | _ -> false
;;

let is_type = function
  | "int" | "bool" | "string" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

(* S1mple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_empty s = parse_white_space *> s <* parse_white_space
let parse_white_space_str str = parse_white_space *> string_ci str <* parse_white_space
let token s = parse_white_space *> s
let token1 s = parse_white_space1 *> s
let stoken s = parse_white_space *> string s
let stoken1 s = parse_white_space1 *> string s
let brackets p = stoken "(" *> p <* stoken ")"
let brackets_or_not p = brackets p <|> p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

(** Const parsers *)

let parse_bool =
  parse_white_space
  *> ((fun _ -> ConstBool true)
      <$> string "true"
      <|> ((fun _ -> ConstBool false) <$> string "false"))
;;

let parse_uint =
  let pd = parse_white_space *> take_while1 is_digit in
  lift (fun digit -> ConstInt (Int.of_string @@ digit)) pd
;;

let parse_int =
  let ps = token (option "" (stoken "-")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> ConstInt (Int.of_string @@ sign ^ digit)) ps pd
;;

let parse_nil = parse_white_space *> ((fun _ -> ConstNil) <$> string "[]")

(* Var parsers *)
let constr_type = (fun _ -> TypeInt) <$> string "int" <|> ((fun _ -> TypeBool) <$> string "bool")
let parse_arrow = parse_empty @@ stoken "->"

let parse_types =
  fix (fun next ->
    lift3 (fun t1 _ t2 -> TypeArrow (t1, t2)) constr_type parse_arrow next <|> constr_type)
;;

let parse_type =
  parse_white_space *> char ':' *> parse_white_space *> parse_types
  <|> ((fun _ -> TypeUnknown) <$> string "")
;;

let check_var cond =
  parse_white_space *> take_while1 cond
  >>= fun v ->
  if is_keyword v
  then fail ("You can not use \"" ^ v ^ "\" keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first symbol is letter, not digit"
  else return v
;;

let parse_var =
  parse_white_space
  *>
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  string "()" *> return "()" <|> check_var is_entry
;;

(** Pattern parsers *)

let parse_rename =
  brackets
  @@ (parse_white_space
      *> choice
           [ string "=" *> return "Eq"
           ; string "<>" *> return "Neq"
           ; string "&&" *> return "And"
           ; string "||" *> return "Or"
           ; string "*" *> return "Mul"
           ; string "/" *> return "Div"
           ; string "%" *> return "Mod"
           ; string "+" *> return "Add"
           ; string "-" *> return "Sub"
           ; string ">=" *> return "Greq"
           ; string ">" *> return "Gre"
           ; string "<=" *> return "Leq"
           ; string "<" *> return "Less"
           ; string "" *> return "()"
           ])
  <* parse_white_space
;;

let parse_pvar =
  brackets_or_not
  @@ (lift
        (fun a ->
          if String.( <> ) a "_"
          then PatVar (a, TypeArrow (TypeInt, TypeArrow (TypeInt, TypeInt)))
          else PatWild)
        parse_rename
      <|> lift2
            (fun a b -> if String.( <> ) a "_" then PatVar (a, b) else PatWild)
            parse_var
            parse_type)
;;

let parse_PatConst =
  (fun v -> PatConst v) <$> choice [ parse_uint; parse_int; parse_bool; parse_nil ]
;;

let parse_wild = (fun _ -> PatWild) <$> stoken "_"

let parse_tuple parser =
  lift2 (fun a b -> PatTuple (a :: b)) (token parser) (many1 (stoken "," *> parser))
;;

let rec constr_con = function
  | [] -> PatConst ConstNil
  | hd :: [] -> hd
  | [ f; s ] -> PatCon (f, s)
  | hd :: tl -> PatCon (hd, constr_con tl)
;;

let parse_con c =
  lift2
    (fun a b -> constr_con @@ (a :: b))
    (c <* stoken "::" <|> (brackets c <* stoken "::"))
    (sep_by (stoken "::") (c <|> brackets c))
;;

let parse_con_2 parser constructor =
  constructor <$> (stoken "[" *> sep_by1 (stoken ";") parser <* stoken "]")
;;

let parse_pattern =
  fix
  @@ fun pack ->
  let value = parse_pvar <|> parse_PatConst in
  let tuple = brackets @@ parse_tuple (value <|> pack) in
  let con =
    parse_con (tuple <|> parse_con_2 pack constr_con <|> value)
    <|> parse_con_2 (tuple <|> pack) constr_con
  in
  choice [ con; tuple; value ]
;;

(** Exp type *)

(* ExpConst *)

let parse_ExpConst = (fun v -> ExpConst v) <$> choice [ parse_uint; parse_bool ]

let parse_ExpConst_int =
  (fun v -> ExpConst v) <$> choice [ parse_uint; parse_int; parse_bool ]
;;

(* ExpVar *)

let parse_ExpVar =
  brackets @@ lift2 (fun a b -> ExpVar (a, b)) parse_var parse_type
  <|> lift (fun a -> ExpVar (a, TypeUnknown)) parse_var
;;

(* ExpBinaryOp *)

let parse_op char_op op = stoken char_op *> return (fun e1 e2 -> ExpBinaryOp (op, e1, e2))
let pmulti = parse_op "*" Mul <|> parse_op "/" Div
let padd = parse_op "+" Add <|> parse_op "-" Sub

let pcomp =
  parse_op ">=" Greq <|> parse_op ">" Gre <|> parse_op "<=" Leq <|> parse_op "<" Less
;;

let peq = parse_op "==" Eq <|> parse_op "=" Eq <|> parse_op "<>" Neq
let patConj = parse_op "&&" And
let pdisj = parse_op "||" Or

let parse_expbinop x =
  let multi = chainl1 x pmulti in
  let add = chainl1 multi padd in
  let comp = chainl1 add pcomp in
  let eq = chainl1 comp peq in
  let conj = chainl1 eq patConj in
  chainl1 conj pdisj <* parse_white_space
;;

(* ExpIfElse *)

let parse_ExpIfElse i expr =
  lift3
    (fun e1 e2 e3 -> ExpIfElse (e1, e2, e3))
    (stoken "if" *> i)
    (stoken "then" *> expr)
    (stoken "else" *> expr)
;;

(* ExpFun *)

let constr_ExpFun pl e = List.fold_right ~init:e ~f:(fun p e -> ExpFun (p, e)) pl
let parse_fun_args = fix @@ fun p -> many1 parse_pattern <|> brackets p

let parse_ExpFun expr =
  brackets_or_not
  @@ lift2 constr_ExpFun (stoken "fun" *> parse_fun_args) (stoken "->" *> expr)
;;

(* ExpApp *)

let parse_ExpApp e1 e2 =
  (* Парсим функцию и аргументы *)
  parse_ExpVar
  <|> e1
  >>= fun f ->
  many1 (parse_ExpVar <|> e2)
  >>= fun args ->
  (* Строим выражение без типов *)
  let app_without_type =
    List.fold_left
      ~f:(fun f_acc arg -> ExpApp (f_acc, arg, TypeUnknown))
        (* Используем TypeUnknown как временный тип *)
      ~init:f
      args
  in
  (* Парсим тип для внешней аппликации *)
  parse_type
  >>= fun ty ->
  (* Заменяем TypeUnknown на вычисленный тип для внешней аппликации *)
  let replace_outer_type expr new_ty =
    match expr with
    | ExpApp (e1, e2, TypeUnknown) ->
      ExpApp (e1, e2, new_ty) (* Заменяем тип только для внешней аппликации *)
    | ExpApp (e1, e2, _) -> ExpApp (e1, e2, TypeUnknown)
    | _ -> expr
  in
  return (replace_outer_type app_without_type ty)
;;

(* ExpLetIn *)

let parse_rec =
  option "false" (stoken1 "rec ")
  >>| fun x -> if String.( <> ) x "false" then Rec else Notrec
;;

let parse_name = parse_rename <|> parse_var

let parse_ExpLetIn expr =
  let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
  lift5
    (fun is_rec name args expr1 expr2 ->
      let expr = constr_ExpFun args expr1 in
      ExpLetIn (is_rec, name, expr, expr2))
    (parse_white_space *> stoken "let" *> parse_rec)
    parse_name
    (many parse_pattern)
    (stoken "=" *> expr)
    (stoken "in" *> expr)
  <|> lift3
        (fun names expr1 expr2 -> ExpLetPatIn (names, expr1, expr2))
        (parse_white_space *> stoken "let" *> parse_pattern)
        (stoken "=" *> expr)
        (stoken "in" *> expr)
;;

(* ExpList *)

let parse_cons_semicolon_expr parser constructor =
  constructor <$> (stoken "[" *> sep_by (stoken ";") parser <* stoken "]")
;;

let rec create_cons_sc = function
  | [] -> ExpConst ConstNil
  | hd :: [] when equal_expression hd (ExpConst ConstNil) -> ExpConst ConstNil
  | hd :: tl -> ExpList (hd, create_cons_sc tl)
;;

let parse_econ c =
  lift2
    (fun a b -> create_cons_sc @@ (a :: b))
    (c <* parse_white_space <* stoken "::")
    (sep_by1 (parse_white_space *> stoken "::") c)
;;

(* ExpTuple *)

let parse_tuple_expr parser =
  brackets
  @@ lift2
       (fun a b -> ExpTuple (a :: b))
       (parser <* stoken ",")
       (sep_by1 (stoken ",") parser)
;;

(* Expression parsers *)

let parse_Exp =
  fix
  @@ fun pack ->
  let expconst = parse_ExpConst in
  let expvar = parse_ExpVar in
  let tuples =
    parse_tuple_expr
      (expconst
       <|> expvar
       <|> parse_tuple_expr pack
       <|> parse_cons_semicolon_expr pack create_cons_sc)
  in
  let parse_if =
    parse_expbinop pack
    <|> brackets (parse_expbinop pack <|> parse_ExpLetIn pack <|> parse_ExpApp pack pack)
  in
  let expression pack =
    parse_expbinop pack
    <|> parse_ExpApp pack pack
    <|> brackets_or_not @@ parse_ExpIfElse parse_if pack
    <|> parse_ExpFun pack
    <|> parse_ExpLetIn pack
    <|> brackets @@ choice [ tuples; parse_expbinop pack ]
    <|> expvar
    <|> expconst
    <|> tuples
  in
  let app_left pack =
    expvar
    <|> (brackets_or_not @@ parse_ExpIfElse parse_if (expression pack)
         <|> parse_ExpFun pack
         <|> brackets @@ parse_ExpApp pack pack
         <|> parse_ExpLetIn pack)
  in
  let app_right pack =
    brackets_or_not
    @@ (parse_expbinop pack
        <|> brackets_or_not @@ parse_ExpIfElse parse_if (expression pack)
        <|> parse_ExpApp pack pack
        <|> parse_ExpFun pack
        <|> parse_ExpLetIn pack)
    <|> tuples
    <|> expvar
    <|> expconst
  in
  let exapp = brackets_or_not @@ parse_ExpApp (app_left pack) (app_right pack) in
  let exapp_int =
    brackets_or_not
    @@ parse_ExpApp
         (app_left (pack <|> parse_ExpConst_int))
         (app_right (pack <|> parse_ExpConst_int))
  in
  let explists =
    parse_econ (exapp <|> expvar)
    <|> parse_cons_semicolon_expr
          (exapp
           <|> expconst
           <|> expvar
           <|> tuples
           <|> parse_cons_semicolon_expr pack create_cons_sc)
          create_cons_sc
  in
  let expifelse = parse_ExpIfElse parse_if (expression pack) in
  let expfun =
    brackets_or_not @@ parse_ExpFun @@ parse_expbinop pack
    <|> tuples
    <|> parse_tuple_expr (exapp <|> expvar <|> expconst)
    <|> parse_ExpApp (app_left pack) (app_right pack)
    <|> brackets_or_not @@ parse_ExpIfElse parse_if (expression pack)
    <|> parse_ExpFun pack
    <|> parse_ExpLetIn pack
  in
  let expbinop =
    brackets_or_not
    @@ parse_expbinop
         (parse_ExpLetIn pack
          <|> parse_ExpApp (app_left pack) (app_right pack)
          <|> brackets @@ brackets_or_not @@ parse_ExpIfElse parse_if (expression pack)
          <|> brackets @@ parse_expbinop pack
          <|> expvar
          <|> expconst)
  in
  let expletin =
    parse_ExpLetIn (expression pack)
    <|> brackets @@ parse_ExpLetIn pack
    <|> expifelse
  in
  choice
    [ expletin
    ; exapp
    ; expfun
    ; explists
    ; expbinop
    ; expifelse
    ; tuples
    ; expvar
    ; expconst
    ; exapp_int
    ]
;;

(** Binding type *)

let parse_let parse =
  let parse_single_let =
    lift4
      (fun flag name args body ->
        let body = constr_ExpFun args body in
        flag, name, body)
      parse_rec
      parse_pattern
      (parse_white_space *> many (brackets_or_not parse_pattern))
      (stoken "=" *> parse)
  in
  let parse_single_multy_let =
    lift3
      (fun name args body ->
        let body = constr_ExpFun args body in
        name, body)
      parse_pattern
      (parse_white_space *> many (brackets_or_not parse_pattern))
      (stoken "=" *> parse)
  in
  let parse_let_and =
    parse_single_let
    >>= fun (r, p, e) ->
    many (stoken "and" *> parse_single_multy_let)
    >>= fun rest_lets -> return (r, (p, e) :: rest_lets)
  in
  parse_white_space *> stoken "let" *> parse_let_and
  >>= fun (r, lets) -> return (Let (r, lets))
;;

let expr_main = (fun expr -> Exp expr) <$> parse_Exp
let parse_bindings = parse_let parse_Exp <|> expr_main
let parse_statements = many1 (token parse_bindings <* token (many (stoken ";;")))
let parse_str p s = parse_string ~consume:All p s
let parser str = parse_str parse_statements (String.strip str)
open Format

type binder = int [@@deriving eq, show { with_path = false }]

module VarMap = Map.Make(Int)

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TList of ty
  | TTuple of ty list
[@@deriving show { with_path = false }]


let normalize_ty_vars ty =
  let var_map = ref VarMap.empty in
  let next_var = ref 0 in
  
  let rec collect_vars acc = function
    | TVar x -> VarSet.add x acc
    | TArrow (l, r) -> collect_vars (collect_vars acc l) r
    | TList t -> collect_vars acc t
    | TTuple ts -> 
        List.fold_left (fun acc t -> collect_vars acc t) acc ts
    | TPrim _ -> acc
  in
  
  let vars = collect_vars VarSet.empty ty in
  let sorted_vars = List.sort Int.compare (VarSet.elements vars) in
  
  (* Создаем mapping *)
  List.iter (fun var ->
    var_map := VarMap.add var !next_var !var_map;
    incr next_var
  ) sorted_vars;
  
  let rec normalize = function
    | TVar x -> 
        (match VarMap.find_opt x !var_map with
         | Some new_var -> TVar new_var
         | None -> TVar x)
    | TArrow (l, r) -> TArrow (normalize l, normalize r)
    | TList t -> TList (normalize t)
    | TTuple ts -> TTuple (List.map normalize ts)
    | t -> t
  in
  normalize ty

let rec pp_ty fmt ty =
  let normalized_ty = normalize_ty_vars ty in
  pp_ty_internal fmt normalized_ty

and pp_ty_internal fmt = function
  | TPrim x -> fprintf fmt "%s" x
  | TVar x -> 
      let var_name = 
        if x < 26 
        then String.make 1 (Char.chr (97 + x)) 
        else "'" ^ string_of_int (x + 1)  (* для x >= 26 *)
      in
      fprintf fmt "'%s" var_name
  | TArrow (l, r) ->
    (match l, r with
     | TArrow _, _ -> fprintf fmt "(%a) -> %a" pp_ty_internal l pp_ty_internal r
     | _, _ -> fprintf fmt "%a -> %a" pp_ty_internal l pp_ty_internal r)
  | TTuple elems ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty_internal)
      elems
  | TList ty ->
    (match ty with
     | TArrow _ | TTuple _ -> fprintf fmt "(%a) list" pp_ty_internal ty
     | _ -> fprintf fmt "%a list" pp_ty_internal ty)
;;

type error =
  [ `Occurs_check of int * ty
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Not_solo_var
  | `Bad_let
  ]

let pp_error_infer fmt = function
  | `Occurs_check (id, ty) ->
    fprintf fmt "Occurs check failed. Type variable '%d occurs inside %a." id pp_ty ty
  | `No_variable s ->
    Stdlib.Format.fprintf fmt "Typechecker error: undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Stdlib.Format.fprintf
      fmt
      "Typechecker error: unification failed on %a and %a"
      pp_ty
      l
      pp_ty
      r
  | `Not_solo_var -> Stdlib.Format.fprintf fmt "Invalid let rec usage"
  | `Bad_let -> Stdlib.Format.fprintf fmt "Typechecker error: empty pattern"
;;

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = TArrow (l, r)
let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let unit_typ = TPrim "unit"
let tuple_typ t = TTuple t
let list_typ t = TList t

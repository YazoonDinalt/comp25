(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ty

module InferState : sig
  type 'a t

  val bind_infer : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module MapM : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:('b -> int * 'a -> 'b t)
      -> 'b t
  end

  val fresh : int t
  val peek_counter : int t
  val run_infer : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind_infer x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind_infer x ~f
  end

  module MapM = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f acc (key, data))
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let peek_counter : int t = fun last -> last, Result.Ok last
  let run_infer m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_check v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_check v l || occurs_check v r
    | TList t -> occurs_check v t
    | TTuple ts -> List.exists ts ~f:(occurs_check v)
    | TPrim _ -> false
  ;;

  let free_type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple ts -> List.fold ts ~init:acc ~f:helper
      | TList t -> helper acc t
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t InferState.t
  val find : fresh -> t -> ty option
  val apply_subst : t -> ty -> ty
  val unify_types : ty -> ty -> t InferState.t
  val compose : t -> t -> t InferState.t
  val compose_many : t list -> t InferState.t
  val remove : t -> fresh -> t
end = struct
  open InferState
  open InferState.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let bind_var k v =
    if Type.occurs_check k v then fail (`Occurs_check (k, v)) else return (k, v)
  ;;

  let singleton k v =
    let* k, v = bind_var k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find k xs = Base.Map.find xs k
  let remove = Map.remove

  let apply_subst s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TList t -> list_typ (helper t)
      | TTuple ts -> tuple_typ (List.map ~f:helper ts)
      | other -> other
    in
    helper
  ;;

  let rec unify_types l r =
    let unify_lists l1 l2 =
      let subs =
        List.fold2 l1 l2 ~init:(return empty) ~f:(fun subs a b ->
          let* subs = subs in
          let sa = apply_subst subs a in
          let sb = apply_subst subs b in
          let* sub1 = unify_types sa sb in
          compose subs sub1)
      in
      match subs with
      | Ok res -> res
      | Unequal_lengths -> fail (`Unification_failed (l, r))
    in
    match l, r with
    | TVar a, TVar b when Int.equal a b -> return empty
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify_types l1 l2 in
      let* subs2 = unify_types (apply_subst subs1 r1) (apply_subst subs1 r2) in
      compose subs1 subs2
    | TList a, TList b -> unify_types a b
    | TTuple a, TTuple b -> unify_lists a b
    | _ -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match find k s with
    | None ->
      let v = apply_subst s v in
      let* s2 = singleton k v in
      MapM.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply_subst s2 v in
        let* k, v = bind_var k v in
        return (Map.add_exn acc ~key:k ~data:v))
    | Some v2 ->
      let* s2 = unify_types v v2 in
      compose s s2

  and compose s1 s2 = MapM.fold_left s2 ~init:(return s1) ~f:extend

  let compose_many ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc ss ->
      let* acc = acc in
      compose acc ss)
  ;;
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
         let open InferState.Syntax in
         let* acc = acc in
         f acc x)
      acc
      set
  ;;
end

module TypeScheme = struct
  type t = scheme

  let occurs_check v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_check v t
  ;;

  let free_type_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_type_vars t) bs
  ;;

  let apply_subst sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply_subst s2 ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (name, scheme, String.comparator_witness) Map.t

  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)
  let remove = Map.remove
  let empty = Map.empty (module String)

  let free_type_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (TypeScheme.free_type_vars s))
  ;;

  let apply_subst s env = Map.map env ~f:(TypeScheme.apply_subst s)
  let find x env = Map.find env x

  let rec ext (S (sub, type_var) as schema) env_ pat =
    match pat, type_var with
    | PatVar (v, TypeUnknown), _ -> extend env_ (v, schema)
    | PatCon (h, tl), TList t ->
      let env = ext (S (sub, t)) env_ h in
      ext (S (sub, type_var)) env tl
    | PatTuple es, TTuple ts ->
      let new_env =
        List.fold2 es ts ~init:env_ ~f:(fun env_ e t -> ext (S (sub, t)) env_ e)
      in
      (match new_env with
       | Ok env_ -> env_
       | _ -> env_)
    | _ -> env_
  ;;
end

open InferState
open InferState.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate_scheme : scheme -> ty InferState.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
       let* f1 = fresh_var in
       let* s = Subst.singleton name f1 in
       return (Subst.apply_subst s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> TypeScheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_type_vars ty) (TypeEnv.free_type_vars env) in
  S (free, ty)
;;

let generalize_recursive env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let rec ty_from_annotation = function
  | TypeInt -> int_typ
  | TypeBool -> bool_typ
  | TypeArrow (a1, a2) -> arrow (ty_from_annotation a1) (ty_from_annotation a2)
  | TypeUnknown -> failwith "TypeUnknown"
;;

let unify_annotation an ty =
  match an with
  | Some an ->
    let* sub = Subst.unify_types (ty_from_annotation an) ty in
    return (Subst.apply_subst sub ty)
  | None -> return ty
;;

open InferState

let infer_pattern =
  let rec helper env = function
    | PatWild ->
      let* int = fresh_var in
      return (env, int)
    | PatConst c ->
      (match c with
       | ConstInt _ -> return (env, int_typ)
       | ConstBool _ -> return (env, bool_typ)
       | ConstNil ->
         let* int = fresh_var in
         return (env, list_typ int))
    | PatVar ("()", _) -> return (env, unit_typ)
    | PatVar (x, TypeUnknown) ->
      let* int = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, int)) in
      return (env, int)
    | PatCon (p1, p2) ->
      let* env1, t1 = helper env p1 in
      let* env2, t2 = helper env1 p2 in
      let* int = fresh_var in
      let* sub_uni = Subst.unify_types t2 (list_typ int) in
      let t2 = Subst.apply_subst sub_uni t2 in
      let* s3 = Subst.unify_types (list_typ t1) t2 in
      let* final_sub = Subst.compose_many [ s3; sub_uni ] in
      let env = TypeEnv.apply_subst final_sub env2 in
      return (env, Subst.apply_subst final_sub t2)
    | PatTuple pl ->
      let* env, tl =
        List.fold_left
          ~f:(fun acc pat ->
            let* env1, tl = acc in
            let* env2, t = helper env1 pat in
            return (env2, t :: tl))
          ~init:(return (env, []))
          pl
      in
      return (env, tuple_typ (List.rev tl))
    | PatVar (name, an) ->
      let* int = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, int)) in
      let* env1, t1 = return (env, int) in
      let* sub = Subst.unify_types t1 (ty_from_annotation an) in
      let env = TypeEnv.apply_subst sub env1 in
      return (env, Subst.apply_subst sub t1)
  in
  helper
;;

let op_str = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | And -> "And"
  | Or -> "Or"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Gre -> "Gre"
  | Leq -> "Leq"
  | Greq -> "Greq"
;;

let infer_expression =
  let rec helper env = function
    | ExpConst c ->
      (match c with
       | ConstInt _ -> return (Subst.empty, int_typ)
       | ConstBool _ -> return (Subst.empty, bool_typ)
       | ConstNil ->
         let* int = fresh_var in
         return (Subst.empty, list_typ int))
    | ExpVar ("()", _) -> return (Subst.empty, unit_typ)
    | ExpVar (x, TypeUnknown) ->
      (match TypeEnv.find x env with
       | Some s ->
         let* t = instantiate_scheme s in
         return (Subst.empty, t)
       | None -> fail (`No_variable x))
    | ExpIfElse (i, t, e) ->
      let* sub1, t1 = helper env i in
      let* sub2, t2 = helper (TypeEnv.apply_subst sub1 env) t in
      let* sub3, t3 = helper (TypeEnv.apply_subst sub2 env) e in
      let* sub4 = Subst.unify_types t1 bool_typ in
      let* sub5 = Subst.unify_types t2 t3 in
      let* sub = Subst.compose_many [ sub1; sub2; sub3; sub4; sub5 ] in
      return (sub, Subst.apply_subst sub t2)
    | ExpLetIn (Rec, x, e1, e2) ->
      let* int = fresh_var in
      let env1 = TypeEnv.extend env (x, S (VarSet.empty, int)) in
      let* s1, t1 = helper env1 e1 in
      let* s2 = Subst.unify_types (Subst.apply_subst s1 int) t1 in
      let* s3 = Subst.compose s1 s2 in
      let env2 = TypeEnv.apply_subst s3 env in
      let t1 = Subst.apply_subst s3 t1 in
      let s = generalize (TypeEnv.remove env2 x) t1 in
      let env3 = TypeEnv.extend env2 (x, s) in
      let* s4, t2 = helper env3 e2 in
      let* s5 = Subst.compose s3 s4 in
      return (s5, t2)
    | ExpLetIn (Notrec, pat, e1, e2) ->
      let pat = PatVar (pat, TypeUnknown) in
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply_subst s1 env in
      let s = generalize env t1 in
      let* env1, t2 = infer_pattern env pat in
      let env2 = TypeEnv.ext s env1 pat in
      let* sub = Subst.unify_types t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply_subst sub1 env2 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | ExpLetPatIn (pat, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply_subst s1 env in
      let s = generalize env t1 in
      let* env1, t2 = infer_pattern env pat in
      let env2 = TypeEnv.ext s env1 pat in
      let* sub = Subst.unify_types t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply_subst sub1 env2 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | ExpFun (p, e) ->
      let* env, t = infer_pattern env p in
      let* sub, t1 = helper env e in
      return (sub, Subst.apply_subst sub (arrow t t1))
    | ExpTuple el ->
      let* sub, t =
        List.fold_left
          ~f:(fun acc e ->
            let* sub, t = acc in
            let* sub1, t1 = helper env e in
            let* sub2 = Subst.compose sub sub1 in
            return (sub2, t1 :: t))
          ~init:(return (Subst.empty, []))
          el
      in
      return (sub, tuple_typ (List.rev_map ~f:(Subst.apply_subst sub) t))
    | ExpApp (e1, e2, TypeUnknown) ->
      let* int = fresh_var in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply_subst s1 env) e2 in
      let* s3 = Subst.unify_types (arrow t2 int) (Subst.apply_subst s2 t1) in
      let* sub = Subst.compose_many [ s1; s2; s3 ] in
      let t = Subst.apply_subst sub int in
      return (sub, t)
    | ExpVar (name, an) ->
      let* sub1, t1 = helper env (ExpVar (name, TypeUnknown)) in
      let* sub2 = Subst.unify_types t1 (ty_from_annotation an) in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply_subst sub t1)
    | ExpApp (exp1, exp2, an) ->
      let* sub1, t1 = helper env (ExpApp (exp1, exp2, TypeUnknown)) in
      let* sub2 = Subst.unify_types t1 (ty_from_annotation an) in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply_subst sub t1)
    | ExpBinaryOp (op, e1, e2) ->
      let op_name = op_str op in
      (match TypeEnv.find op_name env with
       | Some _ ->
         helper
           env
           (ExpApp
              (ExpApp (ExpVar (op_name, TypeUnknown), e1, TypeUnknown), e2, TypeUnknown))
       | None ->
         let* subst_left, typ_left = helper env e1 in
         let* subst_right, typ_right = helper env e2 in
         (match op with
          | Add | Sub | Mul | Div ->
            let* subst' = Subst.unify_types typ_left (TPrim "int") in
            let* subst'' = Subst.unify_types typ_right (TPrim "int") in
            let* final_subst =
              Subst.compose_many [ subst_left; subst_right; subst'; subst'' ]
            in
            return (final_subst, TPrim "int")
          | And | Or ->
            let* subst' = Subst.unify_types typ_left (TPrim "bool") in
            let* subst'' = Subst.unify_types typ_right (TPrim "bool") in
            let* final_subst =
              Subst.compose_many [ subst_left; subst_right; subst'; subst'' ]
            in
            return (final_subst, TPrim "bool")
          | Eq | Neq | Less | Gre | Leq | Greq ->
            let* subst' = Subst.unify_types typ_left typ_right in
            let* final_subst = Subst.compose_many [ subst_left; subst_right; subst' ] in
            return (final_subst, TPrim "bool")))
    | ExpList (hd, tl) ->
      let* s1, t1 = helper env hd in
      let* s2, t2 = helper (TypeEnv.apply_subst s1 env) tl in
      let* elem_ty = fresh_var in
      let* s3 = Subst.unify_types t2 (list_typ elem_ty) in
      let* s4 = Subst.unify_types t1 elem_ty in
      let* sub = Subst.compose_many [ s1; s2; s3; s4 ] in
      return (sub, list_typ (Subst.apply_subst sub elem_ty))
  in
  helper
;;

let infer_bindings env = function
  | Let (Rec, bindings) ->
    (* Create int type variables for all bindings *)
    let* fresh_vars =
      List.fold_left
        ~f:(fun acc (pat, _) ->
          let* acc_vars = acc in
          let* int = fresh_var in
          match pat with
          | PatVar (x, _) -> return ((x, int) :: acc_vars)
          | _ -> fail `Bad_let)
        ~init:(return [])
        bindings
    in
    (* Extend environment with int type variables *)
    let initial_env =
      List.fold_left
        ~f:(fun acc (x, int) -> TypeEnv.extend acc (x, S (VarSet.empty, int)))
        ~init:env
        fresh_vars
    in
    (* Rest of the function remains the same *)
    let* subst_types =
      List.fold_left
        ~f:(fun acc (pat, exp) ->
          let* acc_subst_types = acc in
          let* s, t = infer_expression initial_env exp in
          match pat with
          | PatVar (x, _) -> return ((x, s, t) :: acc_subst_types)
          | _ -> fail `Not_solo_var)
        ~init:(return [])
        bindings
    in
    let* final_subst =
      List.fold_left
        ~f:(fun acc (x, s, t) ->
          let* acc_subst = acc in
          let int = List.Assoc.find_exn ~equal:String.equal fresh_vars x in
          let* s1 = Subst.unify_types (Subst.apply_subst s int) t in
          let* s2 = Subst.compose acc_subst s in
          let* s3 = Subst.compose s2 s1 in
          return s3)
        ~init:(return Subst.empty)
        subst_types
    in
    let env = TypeEnv.apply_subst final_subst env in
    let final_env =
      List.fold_left
        ~f:(fun acc (x, _, t) ->
          let final_type = Subst.apply_subst final_subst t in
          let scheme = generalize (TypeEnv.remove acc x) final_type in
          TypeEnv.extend acc (x, scheme))
        ~init:env
        subst_types
    in
    return final_env
  | Let (Notrec, [ (pattern_, e) ]) ->
    let* s, type1 = infer_expression env e in
    let env = TypeEnv.apply_subst s env in
    let sc = generalize env type1 in
    let* env1, type2 = infer_pattern env pattern_ in
    let env2 = TypeEnv.ext sc env1 pattern_ in
    let* sub = Subst.unify_types type1 type2 in
    let* sub1 = Subst.compose s sub in
    let env3 = TypeEnv.apply_subst sub1 env2 in
    return env3
  | Exp e ->
    let* _, _ = infer_expression env e in
    return env
  | _ -> fail `Bad_let
;;

let start_env =
  let builtin = [ "print_int", TArrow (TPrim "int", TPrim "unit") ] in
  let env = TypeEnv.empty in
  let bind_infer env id typ = TypeEnv.extend env (id, generalize env typ) in
  List.fold_left builtin ~init:env ~f:(fun env (id, typ) -> bind_infer env id typ)
;;

let infer_simple_expression expr =
  Result.map ~f:snd (run_infer (infer_expression start_env expr))
;;

let infer_statements structure =
  let initial_env = start_env in
  List.fold_left
    structure
    ~init:(return (initial_env, Set.empty (module String)))
    ~f:(fun acc item ->
      let* env, defined_names = acc in
      let* new_env = infer_bindings env item in
      let new_names =
        Base.Map.fold new_env ~init:defined_names ~f:(fun ~key ~data:_ acc_names ->
          if not (Base.Map.mem env key) then Set.add acc_names key else acc_names)
      in
      return (new_env, new_names))
  >>| fun (final_env, defined_names) ->
  Base.Map.filter_keys final_env ~f:(fun key -> Set.mem defined_names key)
;;

let run_infer s = run_infer (infer_statements s)

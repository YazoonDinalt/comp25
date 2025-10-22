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
  type 'a t = { run : int -> int * ('a, error) Result.t }

  let run m = m.run
  let return x = { run = (fun st -> st, Result.return x) }
  let fail e = { run = (fun st -> st, Result.fail e) }

  let bind_infer m ~f =
    { run =
        (fun st ->
          let st', r = run m st in
          match r with
          | Ok a -> run (f a) st'
          | Error e -> st', Error e)
    }
  ;;

  let ( >>= ) m f = bind_infer m ~f

  let ( >>| ) m f =
    { run =
        (fun st ->
          match run m st with
          | st', Ok x -> st', Ok (f x)
          | st', Error e -> st', Error e)
    }
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

  let fresh = { run = (fun last -> last + 1, Ok last) }
  let peek_counter = { run = (fun last -> last, Ok last) }
  let run_infer m = snd (run m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_check var typ =
    match typ with
    | TVar id -> Int.equal id var
    | TArrow (lhs, rhs) -> occurs_check var lhs || occurs_check var rhs
    | TList elem -> occurs_check var elem
    | TTuple elems -> List.exists elems ~f:(occurs_check var)
    | TPrim _ -> false
  ;;

  let rec collect_free acc = function
    | TVar id -> VarSet.add id acc
    | TArrow (lhs, rhs) -> collect_free (collect_free acc lhs) rhs
    | TTuple elems -> List.fold elems ~init:acc ~f:collect_free
    | TList elem -> collect_free acc elem
    | TPrim _ -> acc
  ;;

  let free_type_vars typ = collect_free VarSet.empty typ
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

  let bind_var id ty =
    if Type.occurs_check id ty then fail (`Occurs_check (id, ty)) else return (id, ty)
  ;;

  let singleton id ty =
    let* id, ty = bind_var id ty in
    return (Map.singleton (module Int) id ty)
  ;;

  let find k s = Map.find s k
  let remove = Map.remove

  let apply_subst s =
    let rec aux = function
      | TVar id as orig ->
        (match find id s with
         | None -> orig
         | Some t -> t)
      | TArrow (l, r) -> TArrow (aux l, aux r)
      | TList t -> list_typ (aux t)
      | TTuple ts -> tuple_typ (List.map ~f:aux ts)
      | other -> other
    in
    aux
  ;;

  let rec unify_types l r =
    let unify_lists l1 l2 =
      let subs =
        List.fold2 l1 l2 ~init:(return empty) ~f:(fun acc a b ->
          let* acc = acc in
          let a' = apply_subst acc a in
          let b' = apply_subst acc b in
          let* s = unify_types a' b' in
          compose acc s)
      in
      match subs with
      | Ok s -> s
      | Unequal_lengths -> fail (`Unification_failed (l, r))
    in
    match l, r with
    | TVar a, TVar b when Int.equal a b -> return empty
    | TPrim s1, TPrim s2 when String.equal s1 s2 -> return empty
    | TVar id, t | t, TVar id -> singleton id t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify_types l1 l2 in
      let* s2 = unify_types (apply_subst s1 r1) (apply_subst s1 r2) in
      compose s1 s2
    | TList a, TList b -> unify_types a b
    | TTuple a, TTuple b -> unify_lists a b
    | _ -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match find k s with
    | None ->
      let v = apply_subst s v in
      let* s2 = singleton k v in
      MapM.fold_left s ~init:(return s2) ~f:(fun acc (k_existing, v_existing) ->
        let v_existing = apply_subst s2 v_existing in
        let* k_existing, v_existing = bind_var k_existing v_existing in
        return (Map.add_exn acc ~key:k_existing ~data:v_existing))
    | Some v2 ->
      let* s2 = unify_types v v2 in
      compose s s2

  and compose s1 s2 = MapM.fold_left s2 ~init:(return s1) ~f:extend

  let compose_many subs_list =
    List.fold_left subs_list ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
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

  let apply_subst subst (S (names, ty)) =
    let cleaned_subst = VarSet.fold (fun k s -> Subst.remove s k) names subst in
    S (names, Subst.apply_subst cleaned_subst ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (name, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let remove = Map.remove
  let find key env = Map.find env key
  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)

  let free_type_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (TypeScheme.free_type_vars s))
  ;;

  let apply_subst subst env = Map.map env ~f:(TypeScheme.apply_subst subst)

  let rec ext (S (subst, tvar) as scheme) env pat =
    match pat, tvar with
    | PatVar (v, TypeUnknown), _ -> extend env (v, scheme)
    | PatCon (head, tail), TList t ->
      let env' = ext (S (subst, t)) env head in
      ext (S (subst, tvar)) env' tail
    | PatTuple ps, TTuple ts ->
      (match List.fold2 ps ts ~init:env ~f:(fun e p t -> ext (S (subst, t)) e p) with
       | Ok env' -> env'
       | _ -> env)
    | _ -> env
  ;;
end

open InferState
open InferState.Syntax

let fresh_var : ty InferState.t = fresh >>| fun n -> TVar n

let instantiate_scheme (S (bs, t) : scheme) : ty InferState.t =
  VarSet.fold_left_m
    (fun typ name ->
       let* f1 = fresh_var in
       let* s = Subst.singleton name f1 in
       return (Subst.apply_subst s typ))
    bs
    (return t)
;;

let generalize (env : TypeEnv.t) (ty : Type.t) : TypeScheme.t =
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
  let open InferState.Syntax in
  let rec infer env pat =
    match pat with
    | PatWild ->
      let* var_ty = fresh_var in
      return (env, var_ty)
    | PatConst (ConstInt _) -> return (env, int_typ)
    | PatConst (ConstBool _) -> return (env, bool_typ)
    | PatConst ConstNil ->
      let* elem_ty = fresh_var in
      return (env, list_typ elem_ty)
    | PatVar ("()", _) -> return (env, unit_typ)
    | PatVar (name, TypeUnknown) ->
      let* ty_var = fresh_var in
      let env' = TypeEnv.extend env (name, S (VarSet.empty, ty_var)) in
      return (env', ty_var)
    | PatVar (name, annot) ->
      let* ty_var = fresh_var in
      let env' = TypeEnv.extend env (name, S (VarSet.empty, ty_var)) in
      let* _, ty_actual = return (env', ty_var) in
      let* sub = Subst.unify_types ty_actual (ty_from_annotation annot) in
      let env_final = TypeEnv.apply_subst sub env' in
      return (env_final, Subst.apply_subst sub ty_actual)
    | PatCon (hd, tl) ->
      let* env_hd, ty_hd = infer env hd in
      let* env_tl, ty_tl = infer env_hd tl in
      let* elem_ty = fresh_var in
      let* sub_1 = Subst.unify_types ty_tl (list_typ elem_ty) in
      let ty_tl' = Subst.apply_subst sub_1 ty_tl in
      let* sub_2 = Subst.unify_types (list_typ ty_hd) ty_tl' in
      let* sub_final = Subst.compose_many [ sub_2; sub_1 ] in
      let env_final = TypeEnv.apply_subst sub_final env_tl in
      let result_ty = Subst.apply_subst sub_final ty_tl' in
      return (env_final, result_ty)
    | PatTuple elems ->
      let fold_f acc pat_i =
        let* env_acc, ty_acc = acc in
        let* env_next, ty_next = infer env_acc pat_i in
        return (env_next, ty_next :: ty_acc)
      in
      let* env_final, rev_types =
        List.fold_left ~f:fold_f ~init:(return (env, [])) elems
      in
      let tuple_type = tuple_typ (List.rev rev_types) in
      return (env_final, tuple_type)
  in
  infer
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
  let open InferState.Syntax in
  let compose_and_apply subs ty =
    let* sub = Subst.compose_many subs in
    return (sub, Subst.apply_subst sub ty)
  in
  let rec infer env expr =
    match expr with
    | ExpConst (ConstInt _) -> return (Subst.empty, int_typ)
    | ExpConst (ConstBool _) -> return (Subst.empty, bool_typ)
    | ExpConst ConstNil ->
      let* elem_ty = fresh_var in
      return (Subst.empty, list_typ elem_ty)
    | ExpVar ("()", _) -> return (Subst.empty, unit_typ)
    | ExpVar (x, TypeUnknown) ->
      (match TypeEnv.find x env with
       | Some scheme ->
         let* inst_ty = instantiate_scheme scheme in
         return (Subst.empty, inst_ty)
       | None -> fail (`No_variable x))
    | ExpVar (name, annot) ->
      let* sub1, ty1 = infer env (ExpVar (name, TypeUnknown)) in
      let* sub2 = Subst.unify_types ty1 (ty_from_annotation annot) in
      compose_and_apply [ sub1; sub2 ] ty1
    | ExpIfElse (cond, t_branch, e_branch) ->
      let* s1, t_cond = infer env cond in
      let* s2, t_true = infer (TypeEnv.apply_subst s1 env) t_branch in
      let* s3, t_false = infer (TypeEnv.apply_subst s2 env) e_branch in
      let* s4 = Subst.unify_types t_cond bool_typ in
      let* s5 = Subst.unify_types t_true t_false in
      compose_and_apply [ s1; s2; s3; s4; s5 ] t_true
    | ExpLetIn (Rec, name, e1, e2) ->
      let* var_ty = fresh_var in
      let env_pre = TypeEnv.extend env (name, S (VarSet.empty, var_ty)) in
      let* s1, t1 = infer env_pre e1 in
      let* s2 = Subst.unify_types (Subst.apply_subst s1 var_ty) t1 in
      let* s3 = Subst.compose s1 s2 in
      let env_upd = TypeEnv.apply_subst s3 env in
      let t1' = Subst.apply_subst s3 t1 in
      let scheme = generalize (TypeEnv.remove env_upd name) t1' in
      let env_rec = TypeEnv.extend env_upd (name, scheme) in
      let* s4, t2 = infer env_rec e2 in
      let* s5 = Subst.compose s3 s4 in
      return (s5, t2)
    | ExpLetIn (Notrec, pat_name, e1, e2) ->
      let pat = PatVar (pat_name, TypeUnknown) in
      let* s1, t1 = infer env e1 in
      let env' = TypeEnv.apply_subst s1 env in
      let scheme = generalize env' t1 in
      let* env_pat, t_pat = infer_pattern env' pat in
      let env_ext = TypeEnv.ext scheme env_pat pat in
      let* s_unify = Subst.unify_types t_pat t1 in
      let* s_all = Subst.compose_many [ s1; s_unify ] in
      let env_fin = TypeEnv.apply_subst s_all env_ext in
      let* s2, t2 = infer env_fin e2 in
      let* s_final = Subst.compose s_all s2 in
      return (s_final, t2)
    | ExpLetPatIn (pat, e1, e2) ->
      let* s1, t1 = infer env e1 in
      let env' = TypeEnv.apply_subst s1 env in
      let scheme = generalize env' t1 in
      let* env_pat, t_pat = infer_pattern env' pat in
      let env_ext = TypeEnv.ext scheme env_pat pat in
      let* s_unify = Subst.unify_types t_pat t1 in
      let* s_all = Subst.compose_many [ s1; s_unify ] in
      let env_fin = TypeEnv.apply_subst s_all env_ext in
      let* s2, t2 = infer env_fin e2 in
      let* s_final = Subst.compose s_all s2 in
      return (s_final, t2)
    | ExpFun (p, e) ->
      let* env', arg_ty = infer_pattern env p in
      let* sub, body_ty = infer env' e in
      return (sub, Subst.apply_subst sub (arrow arg_ty body_ty))
    | ExpTuple elems ->
      let fold_fn acc e =
        let* s_acc, tys_acc = acc in
        let* s_i, t_i = infer env e in
        let* s_merge = Subst.compose s_acc s_i in
        return (s_merge, t_i :: tys_acc)
      in
      let* s_all, rev_tys =
        List.fold_left ~init:(return (Subst.empty, [])) ~f:fold_fn elems
      in
      let ty_tuple = tuple_typ (List.rev_map ~f:(Subst.apply_subst s_all) rev_tys) in
      return (s_all, ty_tuple)
    | ExpApp (f_expr, arg_expr, TypeUnknown) ->
      let* res_ty = fresh_var in
      let* s1, t_fun = infer env f_expr in
      let* s2, t_arg = infer (TypeEnv.apply_subst s1 env) arg_expr in
      let* s3 = Subst.unify_types (arrow t_arg res_ty) (Subst.apply_subst s2 t_fun) in
      let* s_final = Subst.compose_many [ s1; s2; s3 ] in
      return (s_final, Subst.apply_subst s_final res_ty)
    | ExpApp (f, arg, annot) ->
      let* s1, t1 = infer env (ExpApp (f, arg, TypeUnknown)) in
      let* s2 = Subst.unify_types t1 (ty_from_annotation annot) in
      compose_and_apply [ s1; s2 ] t1
    | ExpBinaryOp (op, e1, e2) ->
      let op_name = op_str op in
      (match TypeEnv.find op_name env with
       | Some _ ->
         infer
           env
           (ExpApp
              (ExpApp (ExpVar (op_name, TypeUnknown), e1, TypeUnknown), e2, TypeUnknown))
       | None ->
         let* s1, t1 = infer env e1 in
         let* s2, t2 = infer env e2 in
         (match op with
          | Add | Sub | Mul | Div ->
            let* s_int1 = Subst.unify_types t1 int_typ in
            let* s_int2 = Subst.unify_types t2 int_typ in
            let* s_final = Subst.compose_many [ s1; s2; s_int1; s_int2 ] in
            return (s_final, int_typ)
          | And | Or ->
            let* s_b1 = Subst.unify_types t1 bool_typ in
            let* s_b2 = Subst.unify_types t2 bool_typ in
            let* s_final = Subst.compose_many [ s1; s2; s_b1; s_b2 ] in
            return (s_final, bool_typ)
          | Eq | Neq | Less | Gre | Leq | Greq ->
            let* s_cmp = Subst.unify_types t1 t2 in
            let* s_final = Subst.compose_many [ s1; s2; s_cmp ] in
            return (s_final, bool_typ)))
    | ExpList (hd, tl) ->
      let* s1, t_hd = infer env hd in
      let* s2, t_tl = infer (TypeEnv.apply_subst s1 env) tl in
      let* elem_ty = fresh_var in
      let* s3 = Subst.unify_types t_tl (list_typ elem_ty) in
      let* s4 = Subst.unify_types t_hd elem_ty in
      compose_and_apply [ s1; s2; s3; s4 ] (list_typ elem_ty)
  in
  infer
;;

let infer_bindings env =
  let open InferState.Syntax in
  let freshen_rec_names bindings =
    let mk acc (pat, _) =
      let* acc_vars = acc in
      let* tv = fresh_var in
      match pat with
      | PatVar (name, _) -> return ((name, tv) :: acc_vars)
      | _ -> fail `Bad_let
    in
    List.fold_left ~f:mk ~init:(return []) bindings
  in
  let extend_env_with_rec env fresh_vars =
    List.fold_left
      ~init:env
      ~f:(fun acc (name, tv) -> TypeEnv.extend acc (name, S (VarSet.empty, tv)))
      fresh_vars
  in
  let infer_let_rec env bindings =
    let* fresh_vars = freshen_rec_names bindings in
    let env_pre = extend_env_with_rec env fresh_vars in
    let* inferred =
      List.fold_left
        ~init:(return [])
        ~f:(fun acc (pat, expr) ->
          let* acc_data = acc in
          let* s, t = infer_expression env_pre expr in
          match pat with
          | PatVar (name, _) -> return ((name, s, t) :: acc_data)
          | _ -> fail `Not_solo_var)
        bindings
    in
    let* subst_final =
      List.fold_left
        ~init:(return Subst.empty)
        ~f:(fun acc (name, s, t) ->
          let* acc_sub = acc in
          let tv = List.Assoc.find_exn ~equal:String.equal fresh_vars name in
          let* s1 = Subst.unify_types (Subst.apply_subst s tv) t in
          let* s2 = Subst.compose acc_sub s in
          let* s3 = Subst.compose s2 s1 in
          return s3)
        inferred
    in
    let env_subst = TypeEnv.apply_subst subst_final env in
    let env_final =
      List.fold_left
        ~init:env_subst
        ~f:(fun acc (name, _, t) ->
          let t' = Subst.apply_subst subst_final t in
          let scheme = generalize (TypeEnv.remove acc name) t' in
          TypeEnv.extend acc (name, scheme))
        inferred
    in
    return env_final
  in
  let infer_let_nonrec env pattern expr =
    let* s_expr, ty_expr = infer_expression env expr in
    let env_expr = TypeEnv.apply_subst s_expr env in
    let scheme = generalize env_expr ty_expr in
    let* env_pat, ty_pat = infer_pattern env_expr pattern in
    let env_ext = TypeEnv.ext scheme env_pat pattern in
    let* s_unify = Subst.unify_types ty_expr ty_pat in
    let* s_all = Subst.compose s_expr s_unify in
    let env_final = TypeEnv.apply_subst s_all env_ext in
    return env_final
  in
  function
  | Let (Rec, bindings) -> infer_let_rec env bindings
  | Let (Notrec, [ (pat, expr) ]) -> infer_let_nonrec env pat expr
  | Exp expr ->
    let* _, _ = infer_expression env expr in
    return env
  | _ -> fail `Bad_let
;;

let start_env =
  let builtins = [ "print_int", arrow int_typ unit_typ ] in
  let env0 = TypeEnv.empty in
  let add_builtin env (name, ty) =
    let scheme = generalize env ty in
    TypeEnv.extend env (name, scheme)
  in
  List.fold_left builtins ~init:env0 ~f:add_builtin
;;

let infer_simple_expression expr =
  run_infer (infer_expression start_env expr) |> Result.map ~f:snd
;;

let infer_statements stmts =
  let open InferState.Syntax in
  let initial_env = start_env in
  let process_stmt acc stmt =
    let* env, defined = acc in
    let* new_env = infer_bindings env stmt in
    let new_defined =
      Base.Map.fold new_env ~init:defined ~f:(fun ~key ~data:_ acc_names ->
        if Base.Map.mem env key then acc_names else Set.add acc_names key)
    in
    return (new_env, new_defined)
  in
  let* final_env, defined_names =
    List.fold_left
      stmts
      ~init:(return (initial_env, Set.empty (module String)))
      ~f:process_stmt
  in
  let filtered_env = Base.Map.filter_keys final_env ~f:(Set.mem defined_names) in
  return filtered_env
;;

let run_infer stmts = run_infer (infer_statements stmts)

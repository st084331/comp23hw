(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Core
module StringSet = Set.Make (String)

let rec find_free_vars exp bound_vars =
  match exp with
  | EConst _ -> StringSet.empty
  | EUnOp (_, e) -> find_free_vars e bound_vars
  | EVar id -> if Set.mem bound_vars id then StringSet.empty else StringSet.singleton id
  | ELet (bindings, e) ->
    let bound_in_bindings, free_in_bindings =
      List.fold_right
        bindings
        ~init:(bound_vars, StringSet.empty)
        ~f:(fun (is_rec, pat, exp) (bound, free) ->
          let new_bound = if is_rec then Set.add bound (pattern_to_id pat) else bound in
          let free_vars_in_exp = find_free_vars exp new_bound in
          new_bound, Set.union free_vars_in_exp free)
    in
    Set.union (find_free_vars e bound_in_bindings) free_in_bindings
  | EFun (pat, e) ->
    let new_bound = Set.add bound_vars (pattern_to_id pat) in
    find_free_vars e new_bound
  | EIf (e1, e2, e3) ->
    StringSet.union_list
      [ find_free_vars e1 bound_vars
      ; find_free_vars e2 bound_vars
      ; find_free_vars e3 bound_vars
      ]
  | EBinOp (_, e1, e2) ->
    Set.union (find_free_vars e1 bound_vars) (find_free_vars e2 bound_vars)
  | EApp (e1, e2) ->
    Set.union (find_free_vars e1 bound_vars) (find_free_vars e2 bound_vars)

and pattern_to_id = function
  | PtVar id -> id
  | _ -> failwith "pattern_to_id: pattern is not a variable"
;;

let rec closure_convert_exp env exp =
  match exp with
  | EVar id as v -> if Set.mem env id then v else EApp (EVar "closure", v)
  | EFun (pat, e) ->
    let pat_id = pattern_to_id pat in
    let new_env = Set.add env pat_id in
    let free_vars = find_free_vars e new_env in
    let closure_vars = Set.diff free_vars (StringSet.singleton pat_id) in
    if Set.is_empty closure_vars
    then EFun (pat, closure_convert_exp new_env e)
    else (
      let new_env_with_closures = Set.union env closure_vars in
      let body = closure_convert_exp new_env_with_closures e in
      let body_with_bindings =
        Set.fold
          ~init:body
          ~f:(fun acc var ->
            if Set.mem env var
            then acc
            else ELet ([ false, PtVar var, EApp (EVar "closure", EVar var) ], acc))
          closure_vars
      in
      EFun (PtVar "closure", EFun (pat, body_with_bindings)))
  | ELet (bindings, e) ->
    let new_env, new_bindings =
      List.fold_right bindings ~init:(env, []) ~f:(fun (is_rec, pat, exp) (env, acc) ->
        let pat_id = pattern_to_id pat in
        let new_env = if is_rec then Set.add env pat_id else env in
        let new_exp = closure_convert_exp new_env exp in
        let new_env = Set.add env pat_id in
        new_env, (is_rec, pat, new_exp) :: acc)
    in
    ELet (new_bindings, closure_convert_exp new_env e)
  | EIf (e1, e2, e3) ->
    EIf
      (closure_convert_exp env e1, closure_convert_exp env e2, closure_convert_exp env e3)
  | EBinOp (op, e1, e2) ->
    EBinOp (op, closure_convert_exp env e1, closure_convert_exp env e2)
  | EApp (e1, e2) -> EApp (closure_convert_exp env e1, closure_convert_exp env e2)
  | _ -> exp
;;

let closure_convert_prog prog =
  let env = StringSet.empty in
  List.map prog ~f:(function DLet (is_rec, pat, exp) ->
    let exp' = closure_convert_exp env exp in
    DLet (is_rec, pat, exp'))
;;

let closure_convert ast = closure_convert_prog ast

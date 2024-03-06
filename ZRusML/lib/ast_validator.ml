(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let get_validated_name cnt = "ast_" ^ string_of_int cnt

let rec validate_exp env cnt exp =
  match exp with
  | EConst _ -> env, cnt, exp
  | EUnOp (o, e) ->
    let new_env, new_cnt, new_exp = validate_exp env cnt e in
    new_env, new_cnt, EUnOp (o, new_exp)
  | EVar id ->
    (match StringMap.find_opt id env with
     | Some v -> env, cnt, EVar v
     | _ -> env, cnt, exp)
  | EIf (condition, true_branch, false_branch) ->
    let _, new_cnt, new_condition = validate_exp env cnt condition in
    let _, new_cnt, new_true_branch = validate_exp env new_cnt true_branch in
    let _, new_cnt, new_false_branch = validate_exp env new_cnt false_branch in
    env, new_cnt, EIf (new_condition, new_true_branch, new_false_branch)
  | EBinOp (o, e1, e2) ->
    let _, new_cnt, new_e1 = validate_exp env cnt e1 in
    let _, new_cnt, new_e2 = validate_exp env new_cnt e2 in
    env, new_cnt, EBinOp (o, new_e1, new_e2)
  | EApp (e1, e2) ->
    let _, new_cnt, new_e1 = validate_exp env cnt e1 in
    let _, new_cnt, new_e2 = validate_exp env new_cnt e2 in
    env, new_cnt, EApp (new_e1, new_e2)
  | EFun (p, e) ->
    (match p with
     | PtVar id ->
       let new_name = get_validated_name cnt in
       let cnt = cnt + 1 in
       let new_env = StringMap.add id new_name env in
       let _, new_cnt, new_e = validate_exp new_env cnt e in
       env, new_cnt, EFun (PtVar new_name, new_e)
     | orig ->
       let _, new_cnt, new_e = validate_exp env cnt e in
       env, new_cnt, EFun (orig, new_e))
  | ELet (bindings, e) ->
    let (env, cnt), new_bindings =
      List.fold_left_map
        (fun (acc_env, acc_cnt) (is_rec, p, exp) ->
          match p with
          | PtVar id ->
            let new_name = get_validated_name acc_cnt in
            let acc_cnt = acc_cnt + 1 in
            let new_env = StringMap.add id new_name acc_env in
            let validated_env = if is_rec then new_env else acc_env in
            let _, acc_cnt, new_exp = validate_exp validated_env acc_cnt exp in
            (new_env, acc_cnt), (is_rec, PtVar new_name, new_exp)
          | _ ->
            let new_name = get_validated_name acc_cnt in
            let acc_cnt = acc_cnt + 1 in
            let _, acc_cnt, new_exp = validate_exp acc_env acc_cnt exp in
            (acc_env, acc_cnt), (is_rec, PtVar new_name, new_exp))
        (env, cnt)
        bindings
    in
    let new_env, new_cnt, new_e = validate_exp env cnt e in
    new_env, new_cnt, ELet (new_bindings, new_e)
;;

let validate_prog prog =
  let ignore_names = [ "main" ] in
  let st_ignore =
    List.fold_left (fun acc elem -> StringSet.add elem acc) StringSet.empty ignore_names
  in
  let _, new_prog =
    List.fold_left_map
      (fun (acc_env, acc_cnt) (DLet (is_rec, p, exp)) ->
        match p with
        | PtVar id ->
          let new_name =
            match StringSet.find_opt id st_ignore with
            | Some _ -> id
            | _ -> get_validated_name acc_cnt
          in
          let acc_cnt = acc_cnt + 1 in
          let new_env = StringMap.add id new_name acc_env in
          let validated_env = if is_rec then new_env else acc_env in
          let _, acc_cnt, new_exp = validate_exp validated_env acc_cnt exp in
          (new_env, acc_cnt), DLet (is_rec, PtVar new_name, new_exp)
        | _ ->
          let new_name = get_validated_name acc_cnt in
          let acc_cnt = acc_cnt + 1 in
          let _, acc_cnt, new_exp = validate_exp acc_env acc_cnt exp in
          (acc_env, acc_cnt), DLet (is_rec, PtVar new_name, new_exp))
      (StringMap.empty, 0)
      prog
  in
  new_prog
;;

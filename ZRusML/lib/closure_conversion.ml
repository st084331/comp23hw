(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Parser
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* let rec find_ids exp =
  match exp with
  | EVar name -> StringSet.singleton name
  | EUnOp (_, e) | EFun (_, e) -> find_ids e
  | ELet (bindings, e) ->
    let acc = find_ids e in
    List.fold_left
      (fun accum (_, _, elem) -> StringSet.union accum (find_ids elem))
      acc
      bindings
  | EIf (e1, e2, e3) ->
    StringSet.union (StringSet.union (find_ids e1) (find_ids e2)) (find_ids e3)
  | EBinOp (_, e1, e2) | EApp (e1, e2) -> StringSet.union (find_ids e1) (find_ids e2)
  | _ -> StringSet.empty
;;

let rec closure_exp exp replace_map global_env =
  let conversion_exp e = closure_exp e replace_map global_env in
  match exp with
  | EVar name ->
    (match StringMap.find_opt name replace_map with
     | Some v -> find_ids v, v
     | None -> StringSet.singleton name, EVar name)
  | EUnOp (op, e) ->
    let st, ex = conversion_exp e in
    st, EUnOp (op, ex)
  | EIf (e1, e2, e3) ->
    let st1, ex1 = conversion_exp e1 in
    let st2, ex2 = conversion_exp e2 in
    let st3, ex3 = conversion_exp e3 in
    StringSet.union (StringSet.union st1 st2) st3, EIf (ex1, ex2, ex3)
  | EBinOp (op, e1, e2) ->
    let st1, ex1 = conversion_exp e1 in
    let st2, ex2 = conversion_exp e2 in
    StringSet.union st1 st2, EBinOp (op, ex1, ex2)
  | EApp (e1, e2) ->
    let st1, ex1 = conversion_exp e1 in
    let st2, ex2 = conversion_exp e2 in
    StringSet.union st1 st2, EApp (ex1, ex2)
  | EFun (_, _) ->
    let vars_list, body = decompose_fun [] exp in
    let vars_list = List.rev vars_list in
    let vars = StringSet.of_list vars_list in
    let body_vars, new_body = conversion_exp body in
    let add_vars = StringSet.diff body_vars (StringSet.union vars global_env) in
    let new_vars = List.of_seq (StringSet.to_seq add_vars) @ vars_list in
    let rec compose_fun ebody = function
      | hd :: tail -> EFun (PtVar hd, compose_fun ebody tail)
      | _ -> ebody
    in
    let new_exp = compose_fun new_body new_vars in
    let rec compose_app = function
      | hd :: tl -> EApp (compose_app tl, EVar hd)
      | _ -> new_exp
    in
    add_vars, new_exp (* compose_app (List.of_seq (StringSet.to_rev_seq add_vars)) *)
  | ELet (bindings, e) ->
    let (new_map, _, ids), new_bindings =
      List.fold_left_map
        (fun (acc, local, ids) (is_rec, t, elem) ->
          let t_name =
            match t with
            | PtVar nm -> nm
            | _ -> "_"
          in
          let vars, new_acc, new_elem =
            match elem with
            | EFun (_, _) ->
              let vars, _ = decompose_fun [] elem in
              let vars = StringSet.of_list vars in
              let global_env_tmp =
                if is_rec then StringSet.add t_name global_env else global_env
              in
              let add_vars, new_body = closure_exp elem acc global_env_tmp in
              let vars_list = List.of_seq (StringSet.to_rev_seq add_vars) in
              let rec construct_app = function
                | hd :: tl -> EApp (construct_app tl, EVar hd)
                | _ -> EVar t_name
              in
              let new_name = construct_app vars_list in
              let new_acc =
                match t with
                | PtVar pt_name -> StringMap.add pt_name new_name replace_map
                | _ -> acc
              in
              let _ =
                Printf.printf "%STTTTTTTTTT%sTTTTTTTTTTTTTT" t_name (show_exp new_body)
              in
              let _, new_body = closure_exp new_body new_acc global_env_tmp in
              vars, new_acc, new_body
            | _ ->
              let _, new_body = closure_exp elem acc global_env in
              find_ids elem, acc, new_body
          in
          let local = StringSet.add t_name local in
          let new_ids = StringSet.diff (find_ids elem) (StringSet.union local vars) in
          (new_acc, local, StringSet.union new_ids ids), (is_rec, t, new_elem))
        (replace_map, StringSet.empty, StringSet.empty)
        bindings
    in
    let _, new_e = closure_exp e new_map global_env in
    ids, ELet (new_bindings, new_e)
  | _ -> StringSet.empty, exp
;;

let closure_transform prog =
  let get_env env is_rec t =
    match t with
    | PtVar name when is_rec -> StringSet.add name env
    | _ -> env
  in
  let _, bindings =
    List.fold_left_map
      (fun (map, env) (DLet (is_rec, t, elem)) ->
        let env = get_env env is_rec t in
        let new_map, new_elem =
          match elem with
          | EFun (PtVar name, _) ->
            let add_vars, new_body = closure_exp elem map env in
            let vars_list = List.of_seq (StringSet.to_rev_seq add_vars) in
            let rec construct_app = function
              | hd :: tl -> EApp (construct_app tl, EVar hd)
              | _ -> EVar name
            in
            let new_name = construct_app vars_list in
            let new_acc = StringMap.add name new_name map in
            new_acc, new_body
          | _ ->
            let _, new_body = closure_exp elem map env in
            map, new_body
        in
        let new_env = get_env env true t in
        (new_map, new_env), DLet (is_rec, t, new_elem))
      (StringMap.empty, StringSet.empty)
      prog
  in
  bindings
;; *)

let rec decompose_fun pts = function
  | EFun (p, e) -> decompose_fun (p :: pts) e
  | e -> pts, e
;;

let rec transform_exp exp env exp_name =
  match exp with
  | EVar nm ->
    (match StringSet.find_opt nm env with
     | Some _ -> StringSet.empty, exp
     | None -> StringSet.singleton nm, exp)
  | EUnOp (op, e) ->
    let new_env, new_exp = transform_exp e env exp_name in
    new_env, EUnOp (op, new_exp)
  | EIf (e1, e2, e3) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    let env3, exp3 = transform_exp e3 env exp_name in
    StringSet.union (StringSet.union env1 env2) env3, EIf (exp1, exp2, exp3)
  | EBinOp (op, e1, e2) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    StringSet.union env1 env2, EBinOp (op, exp1, exp2)
  | EApp (e1, e2) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    StringSet.union env1 env2, EApp (exp1, exp2)
  | EFun (_, _) ->
    let vars, body = decompose_fun [] exp in
    let args =
      StringSet.of_list
        (List.filter_map
           (fun elem ->
             match elem with
             | PtVar name -> Some name
             | _ -> None)
           vars)
    in
    let body_vars, new_body = transform_exp body args exp_name in
    let body_vars = StringSet.remove exp_name body_vars in
    let body_vars_lst = List.of_seq (StringSet.to_seq body_vars) in
    let pt_vars = List.map (fun elem -> PtVar elem) body_vars_lst @ vars in
    let rec helper_efun = function
      | hd :: tl -> EFun (hd, helper_efun tl)
      | _ -> new_body
    in
    let new_efun = helper_efun pt_vars in
    let rec helper_eapp = function
      | hd :: tl -> EApp (helper_eapp tl, hd)
      | _ -> new_efun
    in
    let new_eapp =
      helper_eapp (List.rev (List.map (fun elem -> EVar elem) body_vars_lst))
    in
    StringSet.diff body_vars env, new_eapp
  | ELet (bindings, e) ->
    let (bindings_acc, final_env), new_bindings =
      List.fold_left_map
        (fun (acc, prev_env) (is_rec, p, ex) ->
          let p_name =
            match p with
            | PtVar name -> name
            | _ -> "_"
          in
          let new_acc = if is_rec then StringSet.add p_name acc else acc in
          let next_env, new_ex = transform_exp ex new_acc p_name in
          let new_acc = StringSet.add p_name acc in
          (new_acc, StringSet.union next_env prev_env), (is_rec, p, new_ex))
        (env, StringSet.empty)
        bindings
    in
    let final_acc, new_e = transform_exp e bindings_acc exp_name in
    StringSet.union final_acc final_env, ELet (new_bindings, new_e)
  | _ -> StringSet.empty, exp
;;

let transform_decls bindings =
  let _, new_bindings =
    List.fold_left_map
      (fun acc (DLet (is_rec, p, ex)) ->
        let p_name =
          match p with
          | PtVar name -> name
          | _ -> "_"
        in
        let new_acc = if is_rec then StringSet.add p_name acc else acc in
        let _, new_ex = transform_exp ex new_acc p_name in
        let new_acc = StringSet.add p_name acc in
        new_acc, (is_rec, p, new_ex))
      StringSet.empty
      bindings
  in
  List.map (fun elem -> DLet elem) new_bindings
;;

let%test _ =
  let code =
    {|
    let a c d =
      let m = c + d in
      let k l = 
        let x = l * 2 in
        let y t = m + t in
        y x
      in
      k (5 + m)
    ;;
  |}
  in
  match parse prog code with
  | Error _ ->
    Printf.printf "PARSER ERROR";
    false
  | Result.Ok res ->
    Printf.printf "-------------\n%s\n----------------" (show_prog res);
    Printf.printf "-------------\n%s\n----------------" (show_prog (transform_decls res));
    false
;;

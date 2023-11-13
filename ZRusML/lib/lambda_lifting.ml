(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Compare

let generate_unique_name original_name counter =
  let new_counter = counter + 1 in
  Printf.sprintf "lifted_%s_%d" original_name new_counter, new_counter
;;

let func_name = function
  | EFun (PtVar name, _) -> name
  | _ -> ""
;;

let rec find_nested_functions = function
  | EFun (_, body) as f -> f :: find_nested_functions body
  | ELet (bindings, body) ->
    List.concat (List.map (fun (_, _, e) -> find_nested_functions e) bindings)
    @ find_nested_functions body
  | EIf (cond, then_branch, else_branch) ->
    find_nested_functions cond
    @ find_nested_functions then_branch
    @ find_nested_functions else_branch
  | EBinOp (_, left, right) -> find_nested_functions left @ find_nested_functions right
  | EUnOp (_, expr) -> find_nested_functions expr
  | EApp (func, arg) -> find_nested_functions func @ find_nested_functions arg
  | _ -> []
;;

(* Helper function to find free variables in an expression *)
let rec find_free_vars expr bound_vars =
  match expr with
  | EVar id -> if List.mem id bound_vars then [] else [ id ]
  | EConst _ -> []
  | EUnOp (_, e) -> find_free_vars e bound_vars
  | EBinOp (_, e1, e2) -> find_free_vars e1 bound_vars @ find_free_vars e2 bound_vars
  | EIf (e1, e2, e3) ->
    find_free_vars e1 bound_vars
    @ find_free_vars e2 bound_vars
    @ find_free_vars e3 bound_vars
  | ELet (bindings, e) ->
    let bound_in_let =
      List.fold_left
        (fun acc (_, pt, _) ->
          match pt with
          | PtVar id -> id :: acc
          | _ -> acc)
        []
        bindings
    in
    let vars_in_let =
      List.fold_left (fun acc (_, _, e) -> find_free_vars e bound_vars @ acc) [] bindings
    in
    vars_in_let @ find_free_vars e (bound_in_let @ bound_vars)
  | EFun (pt, e) ->
    let new_bound_vars =
      match pt with
      | PtVar id -> id :: bound_vars
      | _ -> bound_vars
    in
    find_free_vars e new_bound_vars
  | EApp (e1, e2) -> find_free_vars e1 bound_vars @ find_free_vars e2 bound_vars
;;

let parameterize_function expr =
  match expr with
  | EFun (pt, body) ->
    let free_vars = find_free_vars body [] in
    let new_params = List.map (fun var -> PtVar var) free_vars in
    let combined_params =
      match pt with
      | PtVar _ as original_pt -> original_pt :: new_params
      | PtWild | PtConst _ -> new_params
    in
    let updated_body =
      List.fold_right (fun param acc -> EFun (param, acc)) combined_params body
    in
    EFun (pt, updated_body)
  | _ -> expr
;;

let move_to_top_level program nested_func_expr nested_func_name counter =
  let unique_name, new_counter = generate_unique_name nested_func_name counter in
  let rec replace_nested_func expr =
    match expr with
    | EFun (pt, body) as func ->
      if func_name func = nested_func_name
      then EVar unique_name
      else EFun (pt, replace_nested_func body)
    | ELet (bindings, body) ->
      ELet
        ( List.map (fun (b, p, e) -> b, p, replace_nested_func e) bindings
        , replace_nested_func body )
    | EIf (cond, then_branch, else_branch) ->
      EIf
        ( replace_nested_func cond
        , replace_nested_func then_branch
        , replace_nested_func else_branch )
    | EBinOp (op, left, right) ->
      EBinOp (op, replace_nested_func left, replace_nested_func right)
    | EUnOp (op, expr) -> EUnOp (op, replace_nested_func expr)
    | EApp (func, arg) -> EApp (replace_nested_func func, replace_nested_func arg)
    | other -> other
  in
  let updated_program = List.map replace_nested_func program in
  (DLet (true, PtVar unique_name, nested_func_expr), updated_program), new_counter
;;

let rec update_function_calls lifted_functions expr =
  Printf.printf "Current Expr: %s\n" (print_exp expr);
  match expr with
  | EApp (EVar id, arg) when List.mem_assoc id lifted_functions ->
    let free_vars = List.assoc id lifted_functions in
    let updated_arg = update_function_calls lifted_functions arg in
    let new_call =
      List.fold_left (fun acc var -> EApp (acc, EVar var)) (EVar id) free_vars
    in
    let result_expr = EApp (new_call, updated_arg) in
    Printf.printf "Modified Expr: %s\n" (print_exp result_expr);
    result_expr
  | ELet (bindings, body) ->
    let updated_bindings =
      List.map (fun (b, p, e) -> b, p, update_function_calls lifted_functions e) bindings
    in
    ELet (updated_bindings, update_function_calls lifted_functions body)
  | EIf (cond, then_branch, else_branch) ->
    EIf
      ( update_function_calls lifted_functions cond
      , update_function_calls lifted_functions then_branch
      , update_function_calls lifted_functions else_branch )
  | EBinOp (op, left, right) ->
    EBinOp
      ( op
      , update_function_calls lifted_functions left
      , update_function_calls lifted_functions right )
  | EUnOp (op, exp) -> EUnOp (op, update_function_calls lifted_functions exp)
  | EApp (func, arg) ->
    EApp
      ( update_function_calls lifted_functions func
      , update_function_calls lifted_functions arg )
  | _ -> expr
;;

(* Main function for lambda lifting *)
let lambda_lift program =
  let is_recursive, pattern, expression =
    match program with
    | DLet (is_recursive, pattern, expression) -> is_recursive, pattern, expression
    | _ -> failwith "Unsupported program structure"
  in
  let nested_funcs = find_nested_functions expression in
  let parameterized_funcs, lifted_info =
    List.fold_left
      (fun (funcs, lifted) f ->
        let free_vars = find_free_vars f [] in
        let new_f = parameterize_function f in
        let f_name = func_name f in
        (f_name, new_f, free_vars) :: funcs, (f_name, free_vars) :: lifted)
      ([], [])
      nested_funcs
  in
  let lifted_decls, _, _ =
    List.fold_left
      (fun (decls, prog, cnt) (f_name, f_expr, free_vars) ->
        let (new_decl, updated_prog), new_cnt =
          move_to_top_level prog f_expr f_name cnt
        in
        new_decl :: decls, updated_prog, new_cnt)
      ([], [], 0)
      parameterized_funcs
  in
  let updated_expression = update_function_calls lifted_info expression in
  [ DLet (is_recursive, pattern, updated_expression) ]
;;

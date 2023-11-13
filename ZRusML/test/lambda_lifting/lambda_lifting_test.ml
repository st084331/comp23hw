(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open ZRusML_lib.Ast
open ZRusML_lib.Lambda_lifting

let%test "test generate_unique_name" =
  let name1, counter1 = generate_unique_name "test" 0 in
  let name2, counter2 = generate_unique_name "test" counter1 in
  String.equal name1 "lifted_test_1"
  && counter1 = 1
  && String.equal name2 "lifted_test_2"
  && counter2 = 2
;;

let%test "test func_name" =
  let func_expr = EFun (PtVar "test_func", EConst (CInt 42)) in
  String.equal (func_name func_expr) "test_func"
;;

let%test "test find_nested_functions" =
  let expr = ELet ([], EFun (PtVar "f", EFun (PtVar "g", EConst (CInt 1)))) in
  let nested_funcs = find_nested_functions expr in
  List.length nested_funcs = 2
;;

let%test "test find_free_vars" =
  let expr = ELet ([], EBinOp (Add, EVar "x", EVar "y")) in
  let free_vars = find_free_vars expr [] in
  List.length free_vars = 2
  && List.mem free_vars "x" ~equal:String.equal
  && List.mem free_vars "y" ~equal:String.equal
;;

let%test "test parameterize_function" =
  let expr = EFun (PtVar "f", EBinOp (Add, EVar "x", EVar "y")) in
  let param_expr = parameterize_function expr in
  match param_expr with
  | EFun (_, EFun (_, EFun (_, _))) -> true (* Expecting three nested EFun expressions *)
  | _ -> false
;;

let%test "test move_to_top_level" =
  let nested_func_expr = EFun (PtVar "f", EVar "x") in
  let program = [ ELet ([], EApp (nested_func_expr, EConst (CInt 1))) ] in
  let nested_func_name = "f" in
  let (new_decl, updated_program), _ =
    move_to_top_level program nested_func_expr nested_func_name 0
  in
  match new_decl, updated_program with
  | DLet (_, PtVar name, EFun (PtVar _, EVar "x")), [ ELet (_, EApp (EVar name', _)) ] ->
    String.equal name "lifted_f_1" && String.equal name name'
  | _ -> false
;;

let%test "test update_function_calls with one free variable" =
  let lifted_functions = [ "lifted_f_1", [ "x" ] ] in
  let expr = EApp (EVar "lifted_f_1", EConst (CInt 1)) in
  let updated_expr = update_function_calls lifted_functions expr in
  match updated_expr with
  | EApp (EApp (EVar "lifted_f_1", EVar "x"), EConst (CInt 1)) -> true
  | _ -> false
;;

let%test "test update_function_calls" =
  let lifted_functions = [ "lifted_f_1", [ "x"; "y" ] ] in
  let expr = EApp (EVar "lifted_f_1", EConst (CInt 1)) in
  let updated_expr = update_function_calls lifted_functions expr in
  match updated_expr with
  | EApp (EApp (EApp (EVar "lifted_f_1", EVar "x"), EVar "y"), EConst (CInt 1)) -> true
  | _ -> false
;;

(** let%test "test lambda_lift" =
    let nested_func = EFun (PtVar "g", EBinOp (Add, EVar "g", EVar "x")) in
    let program_expr = EFun (PtVar "f", EApp (nested_func, EConst (CInt 1))) in
    let program = DLet (false, PtWild, program_expr) in
    let lifted_program = lambda_lift program in
    match lifted_program with
    | [ DLet (_, _, EFun (_, EApp (EVar lifted_name, _)))
    ; DLet (_, _, EFun (_, EFun (_, EBinOp (Add, EVar "g", EVar "x"))))
    ]
    when String.is_substring lifted_name ~substring:"lifted_g" -> true
    | _ -> false
    ;; *)

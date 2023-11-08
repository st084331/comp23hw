(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Ast
open ZRusML_lib.Closure_conversion
open ZRusML_lib.Compare
open Core

let rec exp_equal e1 e2 =
  match e1, e2 with
  | EVar id1, EVar id2 -> String.equal id1 id2
  | EFun (pat1, exp1), EFun (pat2, exp2) -> pattern_equal pat1 pat2 && exp_equal exp1 exp2
  | EApp (exp1a, exp1b), EApp (exp2a, exp2b) ->
    exp_equal exp1a exp2a && exp_equal exp1b exp2b
  | _, _ -> false

and pattern_equal pat1 pat2 =
  match pat1, pat2 with
  | PtVar id1, PtVar id2 -> String.equal id1 id2
  | _, _ -> false
;;

let%test "pattern_to_id extracts id from PtVar" =
  let pattern = PtVar "test_id" in
  String.(pattern_to_id pattern = "test_id")
;;

let%test "find_free_vars with EVar not in bound_vars" =
  let exp = EVar "x" in
  let bound_vars = StringSet.empty in
  let free_vars = find_free_vars exp bound_vars in
  Set.equal free_vars (StringSet.singleton "x")
;;

let%test "find_free_vars with EVar in bound_vars" =
  let exp = EVar "x" in
  let bound_vars = StringSet.singleton "x" in
  let free_vars = find_free_vars exp bound_vars in
  Set.is_empty free_vars
;;

let%test "closure_convert_exp with no free variables" =
  let exp = EFun (PtVar "x", EVar "x") in
  let env = StringSet.empty in
  let converted_exp = closure_convert_exp env exp in
  if exp_equal exp converted_exp
  then true
  else (
    Printf.printf "Expected: %s\n" (print_exp exp);
    Printf.printf "Actual: %s\n" (print_exp converted_exp);
    false)
;;

let%test "closure_convert_exp with free variables" =
  let exp = EFun (PtVar "x", EVar "y") in
  let env = StringSet.empty in
  let converted_exp = closure_convert_exp env exp in
  match converted_exp with
  | EFun
      ( PtVar "closure"
      , EFun (PtVar "x", ELet ([ (false, PtVar "y", EApp (EVar "closure", EVar "y")) ], _))
      ) -> true
  | _ ->
    Printf.printf "Unexpected structure: %s\n" (print_exp converted_exp);
    false
;;

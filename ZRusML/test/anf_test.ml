(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Anf
open ZRusML_lib.Ast
open ZRusML_lib.Compare
open OUnit2

(* Auxiliary function to compare the ANF transformation result with the expected output *)
let compare_anf ctxt exp expected =
  let anf_result, _ = exp_to_anf exp in
  let result_str = print_exp anf_result in
  assert_equal ~ctxt expected result_str
;;

(* Test for a simple constant expression *)
let test_const ctxt =
  let exp = EConst (CInt 42) in
  compare_anf ctxt exp "CInt(42)"
;;

(* Test for a unary operation *)
let test_unary_op ctxt =
  let exp = EUnOp (Minus, EConst (CInt 42)) in
  compare_anf ctxt exp "EUnOp(Minus, CInt(42))"
;;

(* Test for a binary operation *)
let test_binary_op ctxt =
  let exp = EBinOp (Add, EConst (CInt 2), EConst (CInt 2)) in
  compare_anf ctxt exp "EBinOp(Add, CInt(2), CInt(2))"
;;

(* Test for a let expression *)
let test_let ctxt =
  let exp = ELet ([ false, PtVar "x", EConst (CInt 5) ], EVar "x") in
  compare_anf ctxt exp "ELet([(false, x, CInt(5))], EVar(x))"
;;

(* Test for a function application *)
let test_fun_app ctxt =
  let exp = EApp (EFun (PtVar "x", EVar "x"), EConst (CInt 10)) in
  compare_anf ctxt exp "EApp(EFun(x, EVar(x)), CInt(10))"
;;

(* Add all tests to a test suite *)
let suite =
  "ANF Tests"
  >::: [ "test_const" >:: test_const
       ; "test_unary_op" >:: test_unary_op
       ; "test_binary_op" >:: test_binary_op
       ; "test_let" >:: test_let
       ; "test_fun_app" >:: test_fun_app
       ]
;;

(* Run the test suite *)
let () = run_test_tt_main suite

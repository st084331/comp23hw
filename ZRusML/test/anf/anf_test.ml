(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open! Core
open ZRusML_lib.Ast
open ZRusML_lib.Anf
open ZRusML_lib.Compare

(* Helper function to convert the ANF result to a string for comparison *)
let anf_to_string (exp, lets) =
  let exp_str = print_exp exp in
  match lets with
  | [] -> exp_str
  | _ ->
    let lets_str = List.map lets ~f:print_let |> String.concat ~sep:", " in
    exp_str ^ " with lets: " ^ lets_str
;;

let%test_unit "ANF transformation of EConst" =
  reset_counter ();
  let exp = EConst (CInt 42) in
  let anf, _ = exp_to_anf exp in
  [%test_eq: string] "CInt(42)" (anf_to_string (anf, []))
;;

let%test_unit "ANF transformation of EUnOp" =
  reset_counter ();
  let exp = EUnOp (Minus, EConst (CInt 42)) in
  let anf, lets = exp_to_anf exp in
  [%test_result: string]
    ~expect:"EVar(v0) with lets: (false, PtVar(v0), EUnOp(Minus, CInt(42)))"
    (anf_to_string (anf, lets))
;;

let%test_unit "ANF transformation of EBinOp" =
  reset_counter ();
  let exp = EBinOp (Add, EConst (CInt 2), EConst (CInt 2)) in
  let anf, lets = exp_to_anf exp in
  [%test_result: string]
    ~expect:"EVar(v0) with lets: (false, PtVar(v0), EBinOp(Add, CInt(2), CInt(2)))"
    (anf_to_string (anf, lets))
;;

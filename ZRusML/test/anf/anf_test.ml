(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Anf
open ZRusML_lib.Anf_pretty
open ZRusML_lib.Parser
open ZRusML_lib.Closure_conversion
open ZRusML_lib.Lambda_lifting
open ZRusML_lib.Ast_validator

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR"
  | Result.Ok res ->
    pp_abinding_list
      Format.std_formatter
      (transform_decls res |> lift_prog |> validate_prog |> anf_program)
;;

let%expect_test "anf test sample" =
  let code =
    {|
    let fac n =
      let rec fack n k =
        if n <= 1 then 1
        else fack (n - 1) (fun m -> k (m * n))
      in
      fack n (fun x -> x)
    ;;

    let ans = fac 5;;
  |}
  in
  helper code;
  [%expect
    {|
    let ast_0 ast_1 ast_2 ast_3 =
        let anf_11 = ast_3 * ast_2 in
        let anf_12 = ast_1 anf_11 in
    anf_12;;

    let rec ast_4 ast_5 ast_6 =
        let anf_4 = ast_5 <= 1 in
        let anf_10 = if anf_4 then 1 else     let anf_5 = ast_5 - 1 in
        let anf_6 = ast_4 anf_5 in
        let anf_7 = ast_0 ast_6 in
        let anf_8 = anf_7 ast_5 in
        let anf_9 = anf_6 anf_8 in
    anf_9 in
    anf_10;;

    let ast_7 ast_8 = ast_8;;

    let ast_9 ast_10 =
        let ast_11 = ast_4 in
        let anf_2 = ast_11 ast_10 in
        let anf_3 = anf_2 ast_7 in
    anf_3;;

    let ast_12 = ast_9;;

    let ast_13 =
        let anf_1 = ast_12 5 in
    anf_1;;
  |}]
;;

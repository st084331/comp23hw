(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Anf
open ZRusML_lib.Anf_pretty
open ZRusML_lib.Ast
open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR"
  | Result.Ok res -> pp_abinding_list Format.std_formatter (anf_program res)
;;

let%expect_test "anf test sample" =
  let code =
    {|
    let ast_0 m = k (m * n);;
    let rec ast_1 n k = if (n <= 1) then 1 else ast_1 (n - 1) ast_0;;
    let ast_2 x = x;;
    let ast_3 n = 
        let rec fack = ast_1 in
        fack n ast_2
    ;;
    let fac = ast_3;;
  |}
  in
  helper code;
  [%expect
    {|
  let ast_0 m =
      let anf_9 = m * n in
      let anf_10 = k anf_9 in
  anf_10;;
  
  let rec ast_1 n k =
      let anf_4 = n <= 1 in
      let anf_5 = n - 1 in
      let anf_6 = ast_1 anf_5 in
      let anf_7 = anf_6 ast_0 in
      let anf_8 = if anf_4 then 1 else anf_7 in
  anf_8;;
  
  let ast_2 x = x;;
  
  let ast_3 n =
      let fack = ast_1 in
      let anf_2 = fack n in
      let anf_3 = anf_2 ast_2 in
  anf_3;;
  
  let fac = ast_3;;
  |}]
;;

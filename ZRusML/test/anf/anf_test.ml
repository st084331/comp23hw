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
    let test_print base exp =
       let _ = print_int (bin_pow base exp) in print_endline
     ;;
     
     let main = 
       let test1 = test_print 32 4 in
       let test2 = test_print 7 4 in
       let test3 = test_print 8 9 in
     test_print 2 10;;
  |}
  in
  helper code;
  [%expect
    {|
    let ast_0 ast_1 ast_2 =
        let anf_15 = ast_0 ast_1 in
        let anf_16 = ast_1 anf_15 in
        let anf_17 = anf_16 ast_2 in
    anf_17;;
    
    let ast_3 ast_4 ast_5 ast_6 ast_7 ast_8 =
        let anf_12 = ast_4 ast_7 in
        let anf_13 = anf_12 ast_8 in
        let anf_14 = ast_6 anf_13 in
        let ast_9 = anf_14 in
    ast_5;;
    
    let ast_10 =
        let anf_9 = ast_3 bin_pow in
        let anf_10 = anf_9 print_endline in
        let anf_11 = anf_10 print_int in
    anf_11;;
    
    let main =
        let anf_1 = ast_10 32 in
        let anf_2 = anf_1 4 in
        let ast_12 = anf_2 in
        let anf_3 = ast_10 7 in
        let anf_4 = anf_3 4 in
        let ast_13 = anf_4 in
        let anf_5 = ast_10 8 in
        let anf_6 = anf_5 9 in
        let ast_14 = anf_6 in
        let anf_7 = ast_10 2 in
        let anf_8 = anf_7 10 in
    anf_8;;
       
  |}]
;;

let%expect_test "binary operators test" =
  let code =
    {|
    let x = -3 + 4 + (5 + 7) / 3 * 4;;
    let main = print_int x;;
  |}
  in
  helper code;
  [%expect
    {|
    let ast_0 ast_1 ast_2 =
        let anf_8 = ast_0 ast_1 in
        let anf_9 = ast_1 anf_8 in
        let anf_10 = anf_9 ast_2 in
    anf_10;;
    
    let ast_3 =
        let anf_2 = -3 in
        let anf_3 = 5 + 7 in
        let anf_4 = anf_3 / 3 in
        let anf_5 = anf_4 * 4 in
        let anf_6 = 4 + anf_5 in
        let anf_7 = anf_2 + anf_6 in
    anf_7;;
    
    let main =
        let anf_1 = print_int ast_3 in
    anf_1;;
  |}]
;;

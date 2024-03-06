(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Parser
open ZRusML_lib.Ast_pretty
open ZRusML_lib.Ast_validator

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter (validate_prog res)
;;

let%expect_test _ =
  let code =
    {|
  let f x y = x + y;;

  let t =
    let m x = f 15 x in
    let tmp = 15 in
  m 14 + tmp
;;
  |}
  in
  helper code;
  [%expect
    {|
let ast_0 ast_1 ast_2 = (ast_1 + ast_2);;
let ast_3 =
    let ast_4 ast_5 = ast_0 15 ast_5 in
    let ast_6 = 15 in
    (ast_4 14 + ast_6)
;;
  |}]
;;

let%expect_test _ =
  let code =
    {|
    let fac n =
      let rec fack n k =
        if n <= 1 then 1
        else fack (n - 1) (fun m -> k (m * n))
      in
      fack n (fun x -> x)
    ;;
  |}
  in
  helper code;
  [%expect
    {|
  let ast_0 ast_1 =
      let rec ast_2 ast_3 ast_4 = if (ast_3 <= 1) then 1 else ast_2 (ast_3 - 1) (fun ast_5 -> ast_4 (ast_5 * ast_3)) in
      ast_2 ast_1 (fun ast_6 -> ast_6)
  ;;
  |}]
;;

let%expect_test _ =
  let code =
    {|
    let rec lambda0 x y = (x + lambda0 y);;
    let rec lambda1 x y = (lambda1 x + (lambda1 y + 3));;
    let lambda2 b c = 
        let t = 15 in
        let rec f = lambda1 in
        f (f x) y
    ;;
    let rec f = lambda0;;
    let a = lambda2;;
  |}
  in
  helper code;
  [%expect
    {|
    let rec ast_0 ast_1 ast_2 = (ast_1 + ast_0 ast_2);;
    let rec ast_3 ast_4 ast_5 = (ast_3 ast_4 + (ast_3 ast_5 + 3));;
    let ast_6 ast_7 ast_8 =
        let ast_9 = 15 in
        let rec ast_10 = ast_3 in
        ast_10 (ast_10 x) y
    ;;
    let rec ast_11 = ast_0;;
    let ast_12 = ast_6;;
  |}]
;;

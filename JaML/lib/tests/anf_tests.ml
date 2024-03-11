(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Pprintanf
open Jaml_lib.Parser
open Result

let run_anf_tests test_case =
  let fmt = Format.std_formatter in
  match parse test_case with
  | Error err -> pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Closure.closure typed_commands
       |> Lambdalift.lambda_lift
       |> Anfconv.anf
       |> Format.printf "%a" pp_anfstatements)
;;

let%expect_test _ =
  let _ =
    let e = "let x = (1 + 2) * 3" in
    run_anf_tests e
  in
  [%expect
    {|
    let x =
        let #binop1 = (1 + 2) in
        let #binop2 = (#binop1 * 3) in #binop2
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let x y = (6 + 9) * (4 + y) / y" in
    run_anf_tests e
  in
  [%expect
    {|
    let x y =
        let #binop1 = (6 + 9) in
        let #binop2 = (4 + y) in
        let #binop3 = (#binop1 * #binop2) in
        let #binop4 = (#binop3 / y) in #binop4
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let test f x = f x 1" in
    run_anf_tests e
  in
  [%expect
    {|
    let test f x =
        let #closure1 = add_args_to_closure(f x 1) in #closure1
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let test f x y = if f 1 2 then x + 1 + 3 else y + 4 + 5" in
    run_anf_tests e
  in
  [%expect
    {|
    let test f x y =
        let #closure1 = add_args_to_closure(f 1 2) in
        let #if6 = (if #closure1 then
        let #binop2 = (x + 1) in
        let #binop3 = (#binop2 + 3) in #binop3 else
        let #binop4 = (y + 4) in
        let #binop5 = (#binop4 + 5) in #binop5) in #if6
    |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let rec fact n acc = if n < 1 then acc else fact (n - 1) (n * acc)
    let fac_tailrec n = fact n 1
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let fact n acc =
        let #binop1 = (n < 1) in
        let #if6 = (if #binop1 then acc else
        let #binop2 = (n * acc) in
        let #binop3 = (n - 1) in
        let #empty_closure4 = make_empty_closure(fact) in
        let #closure5 = add_args_to_closure(#empty_closure4 #binop3 #binop2) in #closure5) in #if6;
    let fac_tailrec n =
        let #empty_closure7 = make_empty_closure(fact) in
        let #closure8 = add_args_to_closure(#empty_closure7 n 1) in #closure8
  |}]
;;

let%expect_test _ =
  let _ =
    let e = "let x = (1 + 2, 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let x =
        let #binop2 = (1 + 2) in
        let #binop3 = (3 + 1) in
        let #tuple1 = (#binop2, #binop3) in #tuple1 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let ((x, s), y) = ((1 ,(2 - 4)), 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let #tuple_out1 =
        let #binop3 = (2 - 4) in
        let #tuple2 = (1, #binop3) in
        let #binop4 = (3 + 1) in
        let #tuple1 = (#tuple2, #binop4) in #tuple1;
    let x =
        let #global_var5 = empty_app(#tuple_out1) in
        let #take6 = take(#global_var5, 0) in
        let #take7 = take(#take6, 0) in #take7;
    let s =
        let #global_var8 = empty_app(#tuple_out1) in
        let #take9 = take(#global_var8, 0) in
        let #take10 = take(#take9, 1) in #take10;
    let y =
        let #global_var11 = empty_app(#tuple_out1) in
        let #take12 = take(#global_var11, 1) in #take12 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|

    let apply2 f fst snd = f fst snd
    let sum a b = a + b
    let x = (apply2 sum 3 4, apply2 sum 1 2)

    |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let apply2 f fst snd =
        let #closure1 = add_args_to_closure(f fst snd) in #closure1;
    let sum a b =
        let #binop2 = (a + b) in #binop2;
    let x =
        let #empty_closure4 = make_empty_closure(sum) in
        let #empty_closure5 = make_empty_closure(apply2) in
        let #closure6 = add_args_to_closure(#empty_closure5 #empty_closure4 3 4) in
        let #empty_closure7 = make_empty_closure(sum) in
        let #empty_closure8 = make_empty_closure(apply2) in
        let #closure9 = add_args_to_closure(#empty_closure8 #empty_closure7 1 2) in
        let #tuple3 = (#closure6, #closure9) in #tuple3
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|

    let apply2 f = f (f (f (1 + 0) (2 + 0)) 3) (4 + 5)

    |} in
    run_anf_tests e
  in
  [%expect
    {|
    let apply2 f =
        let #binop1 = (4 + 5) in
        let #binop2 = (2 + 0) in
        let #binop3 = (1 + 0) in
        let #closure4 = add_args_to_closure(f #binop3 #binop2) in
        let #closure5 = add_args_to_closure(f #closure4 3) in
        let #closure6 = add_args_to_closure(f #closure5 #binop1) in #closure6
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let sum a b = (fun c d e f -> a + d + e + f)
    let partial = sum 1 2 3 4 5 6 
    |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum a b c d e f =
        let #binop1 = (a + d) in
        let #binop2 = (#binop1 + e) in
        let #binop3 = (#binop2 + f) in #binop3;
    let partial =
        let #empty_closure4 = make_empty_closure(sum) in
        let #closure5 = add_args_to_closure(#empty_closure4 1 2 3 4 5 6) in #closure5
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let sum6 a b c d e f = a + b + c + d + e + f
      let sum4 a b c d = sum6 a b c d
      let sum2 a b = sum4 a b
      let rer = sum2 1 2 3 4 5 6
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum6 a b c d e f =
        let #binop1 = (a + b) in
        let #binop2 = (#binop1 + c) in
        let #binop3 = (#binop2 + d) in
        let #binop4 = (#binop3 + e) in
        let #binop5 = (#binop4 + f) in #binop5;
    let sum4 a b c d =
        let #empty_closure6 = make_empty_closure(sum6) in
        let #closure7 = add_args_to_closure(#empty_closure6 a b c d) in #closure7;
    let sum2 a b =
        let #empty_closure8 = make_empty_closure(sum4) in
        let #closure9 = add_args_to_closure(#empty_closure8 a b) in #closure9;
    let rer =
        let #empty_closure10 = make_empty_closure(sum2) in
        let #closure11 = add_args_to_closure(#empty_closure10 1 2 3 4 5 6) in #closure11
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
    let sum_cortage ((a, b), (d, e)) = a + b + d + e
      |} in
    run_anf_tests e
  in
  [%expect
    {|
    let sum_cortage #tuple_arg1 =
        let #take1 = take(#tuple_arg1, 0) in
        let #take2 = take(#take1, 0) in
        let a = #take2 in
        let #take3 = take(#tuple_arg1, 0) in
        let #take4 = take(#take3, 1) in
        let b = #take4 in
        let #take5 = take(#tuple_arg1, 1) in
        let #take6 = take(#take5, 0) in
        let d = #take6 in
        let #take7 = take(#tuple_arg1, 1) in
        let #take8 = take(#take7, 1) in
        let e = #take8 in
        let #binop9 = (a + b) in
        let #binop10 = (#binop9 + d) in
        let #binop11 = (#binop10 + e) in #binop11
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let fac n =
      let rec fack n k =
      if n <= 1 then k 1
      else fack (n-1) ((fun k n m -> k (m * n)) k n) 
      in
      fack n (fun x -> x)
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let #closure_fun1 k n m =
        let #binop1 = (m * n) in
        let #closure2 = add_args_to_closure(k #binop1) in #closure2;
    let #closure_fun2 x = x;
    let fack n k =
        let #binop3 = (n <= 1) in
        let #if10 = (if #binop3 then
        let #closure4 = add_args_to_closure(k 1) in #closure4 else
        let #empty_closure5 = make_empty_closure(#closure_fun1) in
        let #closure6 = add_args_to_closure(#empty_closure5 k n) in
        let #binop7 = (n - 1) in
        let #empty_closure8 = make_empty_closure(fack) in
        let #closure9 = add_args_to_closure(#empty_closure8 #binop7 #closure6) in #closure9) in #if10;
    let fac n =
        let #empty_closure11 = make_empty_closure(#closure_fun2) in
        let #empty_closure12 = make_empty_closure(fack) in
        let #closure13 = add_args_to_closure(#empty_closure12 n #empty_closure11) in #closure13
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let fibo n =
      let rec fibo_cps n acc =
      if n < 3 then acc 1
      else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
      in
      fibo_cps n (fun x -> x)
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let #closure_fun1 x acc y =
        let #binop1 = (x + y) in
        let #closure2 = add_args_to_closure(acc #binop1) in #closure2;
    let #closure_fun2 n fibo_cps acc x =
        let #empty_closure3 = make_empty_closure(#closure_fun1) in
        let #closure4 = add_args_to_closure(#empty_closure3 x acc) in
        let #binop5 = (n - 2) in
        let #closure6 = add_args_to_closure(fibo_cps #binop5 #closure4) in #closure6;
    let #closure_fun3 x = x;
    let fibo_cps n acc =
        let #binop7 = (n < 3) in
        let #if15 = (if #binop7 then
        let #closure8 = add_args_to_closure(acc 1) in #closure8 else
        let #empty_closure9 = make_empty_closure(fibo_cps) in
        let #empty_closure10 = make_empty_closure(#closure_fun2) in
        let #closure11 = add_args_to_closure(#empty_closure10 n #empty_closure9 acc) in
        let #binop12 = (n - 1) in
        let #empty_closure13 = make_empty_closure(fibo_cps) in
        let #closure14 = add_args_to_closure(#empty_closure13 #binop12 #closure11) in #closure14) in #if15;
    let fibo n =
        let #empty_closure16 = make_empty_closure(#closure_fun3) in
        let #empty_closure17 = make_empty_closure(fibo_cps) in
        let #closure18 = add_args_to_closure(#empty_closure17 n #empty_closure16) in #closure18
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let test x y z = (if x > 1 then (if y > 0 then 1 else 2) else (if z > 0 then 3 else 4))
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let test x y z =
        let #binop1 = (x > 1) in
        let #if6 = (if #binop1 then
        let #binop2 = (y > 0) in
        let #if3 = (if #binop2 then 1 else 2) in #if3 else
        let #binop4 = (z > 0) in
        let #if5 = (if #binop4 then 3 else 4) in #if5) in #if6

 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let sum a b = a + b
      let minus a b = a - b
      let test1 = sum
      let test1_res = test1 1 2
      let test2 = let x = 1 in sum
      let test2_res = test2 1 2
      let test3 a = if a = 1 then sum else minus
      let test3_res = test3 0 3 4
      let a = 5
      let b = a
      let c = 5 + a + b
      let sum4 arg = arg + a + b + 1 
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum a b =
        let #binop1 = (a + b) in #binop1;
    let minus a b =
        let #binop2 = (a - b) in #binop2;
    let test1 =
        let #empty_closure3 = make_empty_closure(sum) in #empty_closure3;
    let test1_res =
        let #global_var4 = empty_app(test1) in
        let #closure5 = add_args_to_closure(#global_var4 1 2) in #closure5;
    let test2 =
        let x = 1 in
        let #empty_closure6 = make_empty_closure(sum) in #empty_closure6;
    let test2_res =
        let #global_var7 = empty_app(test2) in
        let #closure8 = add_args_to_closure(#global_var7 1 2) in #closure8;
    let test3 a =
        let #binop9 = (a = 1) in
        let #if12 = (if #binop9 then
        let #empty_closure10 = make_empty_closure(sum) in #empty_closure10 else
        let #empty_closure11 = make_empty_closure(minus) in #empty_closure11) in #if12;
    let test3_res =
        let #empty_closure13 = make_empty_closure(test3) in
        let #closure14 = add_args_to_closure(#empty_closure13 0 3 4) in #closure14;
    let a = 5;
    let b =
        let #global_var15 = empty_app(a) in #global_var15;
    let c =
        let #global_var16 = empty_app(a) in
        let #binop17 = (5 + #global_var16) in
        let #global_var18 = empty_app(b) in
        let #binop19 = (#binop17 + #global_var18) in #binop19;
    let sum4 arg =
        let #global_var20 = empty_app(a) in
        let #binop21 = (arg + #global_var20) in
        let #global_var22 = empty_app(b) in
        let #binop23 = (#binop21 + #global_var22) in
        let #binop24 = (#binop23 + 1) in #binop24



 |}]
;;

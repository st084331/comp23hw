(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Parser
open ZRusML_lib.Ast_pretty
open ZRusML_lib.Lambda_lifting
open ZRusML_lib.Closure_conversion

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter (lift_prog (transform_decls res))
;;

let%expect_test _ =
  let code =
    {|
  let a =
    let m = c + d in
    let k =
      (fun m l ->
        let x = l * 2 in
        let y = (fun m t -> m + t) m in
        y x)
        m
    in
    k (5 + m)
;;
  |}
  in
  helper code;
  [%expect
    {|
  let rec /Z f x = f (/Z f) x;;
  let 0lambda m t = (m + t);;
  let 1lambda m l =
      let x = (l * 2) in
      let y = 0lambda m in
      y x
  ;;
  let a =
      let m = (c + d) in
      let k = 1lambda m in
      k (5 + m)
  ;;
  |}]
;;

let%expect_test _ =
  let code = {|
    let f = (fun y x -> (x + y)) y;;
  |} in
  helper code;
  [%expect
    {|
  let rec /Z f x = f (/Z f) x;;
  let 0lambda y x = (x + y);;
  let f = 0lambda y;;
  |}]
;;

let%expect_test _ =
  let code =
    {|
    let fac n =
      let rec fack n k =
        if n <= 1 then k 1
        else fack (n - 1) (fun m -> k (m * n))
      in
      fack n (fun x -> x)
;;
|}
  in
  helper code;
  [%expect
    {|
  let rec /Z f x = f (/Z f) x;;
  let 0lambda k n m = k (m * n);;
  let 1lambda fack n k = if (n <= 1) then k 1 else fack (n - 1) (0lambda k n);;
  let 2lambda fack = 1lambda fack;;
  let 3lambda x = x;;
  let 4lambda n =
      let fack = /Z 2lambda in
      fack n 3lambda
  ;;
  let fac = 4lambda;;
|}]
;;

let%expect_test _ =
  let code =
    {|
    let f_wrapper f n = if n <= 1 then 0 else ((fun y -> y 15 + f (n - 1)) (fun t -> 15 * t + f (n - 1)));;
    let rec f n = f_wrapper f n;;
|}
  in
  helper code;
  [%expect
    {|
  let rec /Z f x = f (/Z f) x;;
  let 0lambda f n y = (y 15 + f (n - 1));;
  let 1lambda f n t = ((15 * t) + f (n - 1));;
  let 2lambda f n = if (n <= 1) then 0 else 0lambda f n (1lambda f n);;
  let f_wrapper = 2lambda;;
  let 3lambda f f_wrapper n = f_wrapper f n;;
  let 4lambda f = 3lambda f f_wrapper;;
  let f = /Z 4lambda;;
|}]
;;

let%expect_test _ =
  let code =
    {|
    let fibo n =
      let rec fibo_cps n acc = if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
    in
      fibo_cps n (fun x -> x)
    ;;
|}
  in
  helper code;
  [%expect
    {|
  let rec /Z f x = f (/Z f) x;;
  let 0lambda acc x y = acc (x + y);;
  let 1lambda acc fibo_cps n x = fibo_cps (n - 2) (0lambda acc x);;
  let 2lambda fibo_cps n acc = if (n < 3) then acc 1 else fibo_cps (n - 1) (1lambda acc fibo_cps n);;
  let 3lambda fibo_cps = 2lambda fibo_cps;;
  let 4lambda x = x;;
  let 5lambda n =
      let fibo_cps = /Z 3lambda in
      fibo_cps n 4lambda
  ;;
  let fibo = 5lambda;;
|}]
;;

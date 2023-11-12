(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter res
;;

let%expect_test "function test" =
  let code = {|
    let f x = x + y;;
|} in
  helper code;
  [%expect {|
  let f = (fun x -> (x + y));;
|}]
;;

let%expect_test "let in, apps, operators test" =
  let code =
    {|
    let a c d =
      let m = c + d in
      let k l = l + m in
      k (5 + m)
    ;;
|}
  in
  helper code;
  [%expect
    {|
let a = (fun c d -> (
    let m = (c + d) in
    let k = (fun l -> (l + m)) in
    k (5 + m)
) );;
|}]
;;

let%expect_test "let in depth 2, apps, operators" =
  let code =
    {|
    let a c d =
      let m = c + d in
      let k l = 
        let x = l * 2 in
        let y t = m + t in
        y x
      in
      k (5 + m)
    ;;
|}
  in
  helper code;
  [%expect
    {|
  let a = (fun c d -> (
      let m = (c + d) in
      let k = (fun l -> (
          let x = (l * 2) in
          let y = (fun t -> (m + t)) in
          y x
      ) ) in
      k (5 + m)
  ) );;
|}]
;;

let%expect_test "factorial cps test" =
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
let fac = (fun n -> (
    let rec fack = (fun n k -> if n <= 1 then 1 else fack (n - 1) (fun m -> k (m * n))) in
    fack n (fun x -> x)
) );;
|}]
;;

let%expect_test "let ins test" =
  let code =
    {|
    let f a b = 
      let x y = y + z in
      let t = 
        let g s = m * s in
        g 15
      in
      x 1337
    ;;
|}
  in
  helper code;
  [%expect
    {|
let f = (fun a b -> (
    let x = (fun y -> (y + z)) in
    let t = (
        let g = (fun s -> (m * s)) in
        g 15
    )  in
    x 1337
) );;
|}]
;;

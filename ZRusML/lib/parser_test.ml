(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Parser

let test_parse ~label ~code ~expected =
  match parse prog code with
  | Error _ ->
    Printf.printf "[Parser test] %s -> PARSE ERROR" label;
    false
  | Result.Ok res when expected = res -> true
  | Result.Ok res ->
    let () =
      Printf.printf "[Parser test] %s failed.\nActual is:\n%s\n" label (show_prog res)
    in
    false
;;

let%test _ =
  test_parse
    ~label:"Fun and val test"
    ~code:
      {|

    let f = fun x -> fun y -> fun z -> (x + y * z) / 2 * 1337 + 9 / 13 - 14;;

    let f x y z = (x + y * z) / 2 * 1337 + 9 / 13 - 14;;

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ; DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ]
;;

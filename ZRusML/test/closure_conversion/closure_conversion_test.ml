(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Ast
open ZRusML_lib.Closure_conversion
open ZRusML_lib.Compare
open Core

let%test "pattern_to_id extracts id from PtVar" =
  let pattern = PtVar "test_id" in
  String.(pattern_to_id pattern = "test_id")
;;

let%test "find_free_vars with EVar not in bound_vars" =
  let exp = EVar "x" in
  let bound_vars = StringSet.empty in
  let free_vars = find_free_vars exp bound_vars in
  Set.equal free_vars (StringSet.singleton "x")
;;

let%test "find_free_vars with EVar in bound_vars" =
  let exp = EVar "x" in
  let bound_vars = StringSet.singleton "x" in
  let free_vars = find_free_vars exp bound_vars in
  Set.is_empty free_vars
;;

let%test "closure_convert_exp with no free variables" =
  let env = StringSet.empty in
  let converted_exp = closure_convert_exp env (EFun (PtVar "x", EVar "x")) in
  match converted_exp with
  | EFun (PtVar "x", EVar "x") -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_exp (EFun (PtVar "x", EVar "x")));
    Printf.printf "Actual: %s\n" (print_exp converted_exp);
    false
;;

let%test "closure_convert_exp with free variables" =
  let exp = EFun (PtVar "x", EVar "y") in
  let env = StringSet.empty in
  let converted_exp = closure_convert_exp env exp in
  match converted_exp with
  | EFun
      ( PtVar "closure"
      , EFun (PtVar "x", ELet ([ (false, PtVar "y", EApp (EVar "closure", EVar "y")) ], _))
      ) -> true
  | _ ->
    Printf.printf "Unexpected structure: %s\n" (print_exp converted_exp);
    false
;;

let%test "closure_convert_prog with multiple declarations" =
  let prog =
    [ DLet (false, PtVar "x", EVar "y")
    ; DLet (true, PtVar "f", EFun (PtVar "z", EVar "x"))
    ; DLet (false, PtVar "y", EConst (CInt 5))
    ]
  in
  let converted_prog = closure_convert_prog prog in
  let expected =
    [ DLet (false, PtVar "x", EApp (EVar "closure", EVar "y"))
    ; DLet
        ( true
        , PtVar "f"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "z"
                , ELet ([ false, PtVar "x", EApp (EVar "closure", EVar "x") ], EVar "x")
                ) ) )
    ; DLet (false, PtVar "y", EConst (CInt 5))
    ]
  in
  match converted_prog with
  | [ DLet (false, PtVar "x", EApp (EVar "closure", EVar "y"))
    ; DLet
        ( true
        , PtVar "f"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "z"
                , ELet ([ (false, PtVar "x", EApp (EVar "closure", EVar "x")) ], EVar "x")
                ) ) )
    ; DLet (false, PtVar "y", EConst (CInt 5))
    ] -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_prog expected);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

(* Test for a function with multiple free variables *)
let%test "closure_convert_prog with function multiple free vars" =
  let prog =
    [ DLet (false, PtVar "f", EFun (PtVar "x", EBinOp (Add, EVar "x", EVar "y"))) ]
  in
  let converted_prog = closure_convert_prog prog in
  let expected =
    [ DLet
        ( false
        , PtVar "f"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "x"
                , ELet
                    ( [ false, PtVar "y", EApp (EVar "closure", EVar "y") ]
                    , EBinOp (Add, EVar "x", EVar "y") ) ) ) )
    ]
  in
  match converted_prog with
  | [ DLet
        ( false
        , PtVar "f"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "x"
                , ELet
                    ( [ (false, PtVar "y", EApp (EVar "closure", EVar "y")) ]
                    , EBinOp (Add, EVar "x", EVar "y") ) ) ) )
    ] -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_prog expected);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

(* Test for nested functions *)
let%test "closure_convert_prog with nested functions" =
  let prog =
    [ DLet
        ( false
        , PtVar "g"
        , EFun (PtVar "x", EFun (PtVar "y", EBinOp (Add, EVar "x", EVar "y"))) )
    ]
  in
  let converted_prog = closure_convert_prog prog in
  let expected =
    [ DLet
        ( false
        , PtVar "g"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "x"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "y"
                        , ELet
                            ( [ false, PtVar "x", EApp (EVar "closure", EVar "x") ]
                            , EBinOp (Add, EVar "x", EVar "y") ) ) ) ) ) )
    ]
  in
  match converted_prog with
  | [ DLet
        ( false
        , PtVar "g"
        , EFun
            ( PtVar "closure"
            , EFun
                ( PtVar "x"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "y"
                        , ELet
                            ( [ (false, PtVar "x", EApp (EVar "closure", EVar "x")) ]
                            , EBinOp (Add, EVar "x", EVar "y") ) ) ) ) ) )
    ] -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_prog expected);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

(* Test for a let expression with a function that has a free variable *)
let%test "closure_convert_prog with let expression and free variable" =
  let prog =
    [ DLet
        ( false
        , PtVar "h"
        , ELet ([ false, PtVar "i", EFun (PtVar "j", EVar "k") ], EVar "i") )
    ]
  in
  let converted_prog = closure_convert_prog prog in
  let expected =
    [ DLet
        ( false
        , PtVar "h"
        , ELet
            ( [ ( false
                , PtVar "i"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "j"
                        , ELet
                            ( [ false, PtVar "k", EApp (EVar "closure", EVar "k") ]
                            , EVar "k" ) ) ) )
              ]
            , EVar "i" ) )
    ]
  in
  match converted_prog with
  | [ DLet
        ( false
        , PtVar "h"
        , ELet
            ( [ ( false
                , PtVar "i"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "j"
                        , ELet
                            ( [ (false, PtVar "k", EApp (EVar "closure", EVar "k")) ]
                            , EVar "k" ) ) ) )
              ]
            , EVar "i" ) )
    ] -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_prog expected);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

(* Test for a complex expression with multiple nested lets and functions *)
let%test "closure_convert_prog with complex nested lets and functions" =
  let prog =
    [ DLet
        ( false
        , PtVar "complex"
        , ELet
            ( [ false, PtVar "a", EConst (CInt 1)
              ; true, PtVar "b", EFun (PtVar "x", EBinOp (Add, EVar "a", EVar "x"))
              ]
            , EVar "b" ) )
    ]
  in
  let converted_prog = closure_convert_prog prog in
  let expected =
    [ DLet
        ( false
        , PtVar "complex"
        , ELet
            ( [ false, PtVar "a", EConst (CInt 1)
              ; ( true
                , PtVar "b"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "x"
                        , ELet
                            ( [ false, PtVar "a", EApp (EVar "closure", EVar "a") ]
                            , EBinOp (Add, EVar "a", EVar "x") ) ) ) )
              ]
            , EVar "b" ) )
    ]
  in
  match converted_prog with
  | [ DLet
        ( false
        , PtVar "complex"
        , ELet
            ( [ (false, PtVar "a", EConst (CInt 1))
              ; ( true
                , PtVar "b"
                , EFun
                    ( PtVar "closure"
                    , EFun
                        ( PtVar "x"
                        , ELet
                            ( [ (false, PtVar "a", EApp (EVar "closure", EVar "a")) ]
                            , EBinOp (Add, EVar "a", EVar "x") ) ) ) )
              ]
            , EVar "b" ) )
    ] -> true
  | _ ->
    Printf.printf "Expected: %s\n" (print_prog expected);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

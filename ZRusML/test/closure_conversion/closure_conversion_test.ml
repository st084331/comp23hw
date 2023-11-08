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
    Printf.printf
      "Expected: %s\n"
      (print_prog
         [ DLet (false, PtVar "x", EApp (EVar "closure", EVar "y"))
         ; DLet
             ( true
             , PtVar "f"
             , EFun
                 ( PtVar "closure"
                 , EFun
                     ( PtVar "z"
                     , ELet
                         ([ false, PtVar "x", EApp (EVar "closure", EVar "x") ], EVar "x")
                     ) ) )
         ; DLet (false, PtVar "y", EConst (CInt 5))
         ]);
    Printf.printf "Actual: %s\n" (print_prog converted_prog);
    false
;;

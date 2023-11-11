(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ll_ast
open Anf
open Counter

let gen_var = gen_var "anf"

let rec anf (e : ll_expr) (expr_with_hole : immexpr -> aexpr) =
  match e with
  | LLiteral (LInt n) -> expr_with_hole (ImmInt n)
  | LLiteral (LBool n) -> expr_with_hole (ImmBool n)
  | LIdentifier x -> expr_with_hole (ImmIdentifier x)
  | LUnaryOp (op, body) ->
    let varname = gen_var () in
    anf body (fun imm ->
      ALet (varname, CUnaryOp (op, imm), expr_with_hole (ImmIdentifier varname)))
  | LBinaryOp (op, left, right) ->
    let varname = gen_var () in
    anf left (fun limm ->
      anf right (fun rimm ->
        ALet (varname, CBinaryOp (op, limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | LApp (left, right) ->
    anf left (fun limm ->
      anf right (fun rimm ->
        let varname = gen_var () in
        ALet (varname, CApp (limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | LIfThenElse (condition, true_branch, false_branch) ->
    anf condition (fun cond ->
      anf true_branch (fun t_branch ->
        anf false_branch (fun f_branch -> ACExpr (CIfThenElse (cond, t_branch, f_branch)))))
  | LLetIn (id, left, right) ->
    anf left (fun limm ->
      anf right (fun rimm -> ALet (id, CImmExpr limm, expr_with_hole rimm)))
;;

let anf_binding = function
  | LVal (id, body) -> AVal (id, anf body (fun imm -> ACExpr (CImmExpr imm)))
  | LFun (id, args, body) -> AFun (id, args, anf body (fun imm -> ACExpr (CImmExpr imm)))
;;

let anf_program program =
  reset ();
  List.map anf_binding program
;;

(* tests *)
let equal anf_program1 cc_program2 = Base.Poly.equal anf_program1 cc_program2

(* fack1 k n m = k (m * n)
   fack1 k n m =
   let anf_1 = m * n in
   let anf_2 = k anf_1
   in anf_2
*)
let%test _ =
  let ll =
    [ LFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , LApp (LIdentifier "k", LBinaryOp (Mult, LIdentifier "m", LIdentifier "n")) )
    ; LFun ("ll_1", [ "x" ], LIdentifier "x")
    ; LFun ("fac", [ "n" ], LApp (LIdentifier "n", LIdentifier "ll_1"))
    ]
  in
  let anf =
    [ AFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , ALet
            ( "anf_1"
            , CBinaryOp (Mult, ImmIdentifier "m", ImmIdentifier "n")
            , ALet
                ( "anf_2"
                , CApp (ImmIdentifier "k", ImmIdentifier "anf_1")
                , ACExpr (CImmExpr (ImmIdentifier "anf_2")) ) ) )
    ; AFun ("ll_1", [ "x" ], ACExpr (CImmExpr (ImmIdentifier "x")))
    ; AFun
        ( "fac"
        , [ "n" ]
        , ALet
            ( "anf_3"
            , CApp (ImmIdentifier "n", ImmIdentifier "ll_1")
            , ACExpr (CImmExpr (ImmIdentifier "anf_3")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

(*add a b = let anf_2 = ~a in let anf_1 = anf_2 + b in anf_1*)
let%test _ =
  let ll =
    [ LFun
        ( "add"
        , [ "a"; "b" ]
        , LBinaryOp (Add, LUnaryOp (Neg, LIdentifier "a"), LIdentifier "b") )
    ]
  in
  let anf =
    [ AFun
        ( "add"
        , [ "a"; "b" ]
        , ALet
            ( "anf_2"
            , CUnaryOp (Neg, ImmIdentifier "a")
            , ALet
                ( "anf_1"
                , CBinaryOp (Add, ImmIdentifier "anf_2", ImmIdentifier "b")
                , ACExpr (CImmExpr (ImmIdentifier "anf_1")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll =
    [ LFun ("add", [ "a"; "b" ], LBinaryOp (Add, LIdentifier "a", LIdentifier "b")) ]
  in
  let anf =
    [ AFun
        ( "add"
        , [ "a"; "b" ]
        , ALet
            ( "anf_1"
            , CBinaryOp (Add, ImmIdentifier "a", ImmIdentifier "b")
            , ACExpr (CImmExpr (ImmIdentifier "anf_1")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll = [ LFun ("neg", [ "x" ], LUnaryOp (Neg, LIdentifier "x")) ] in
  let anf =
    [ AFun
        ( "neg"
        , [ "x" ]
        , ALet
            ( "anf_1"
            , CUnaryOp (Neg, ImmIdentifier "x")
            , ACExpr (CImmExpr (ImmIdentifier "anf_1")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll =
    [ LFun ("foo", [ "x"; "y" ], LBinaryOp (Add, LIdentifier "x", LIdentifier "y"))
    ; LFun
        ( "bar"
        , [ "a"; "b"; "c" ]
        , LApp (LIdentifier "a", LBinaryOp (Sub, LIdentifier "b", LIdentifier "c")) )
    ]
  in
  let anf =
    [ AFun
        ( "foo"
        , [ "x"; "y" ]
        , ALet
            ( "anf_1"
            , CBinaryOp (Add, ImmIdentifier "x", ImmIdentifier "y")
            , ACExpr (CImmExpr (ImmIdentifier "anf_1")) ) )
    ; AFun
        ( "bar"
        , [ "a"; "b"; "c" ]
        , ALet
            ( "anf_2"
            , CBinaryOp (Sub, ImmIdentifier "b", ImmIdentifier "c")
            , ALet
                ( "anf_3"
                , CApp (ImmIdentifier "a", ImmIdentifier "anf_2")
                , ACExpr (CImmExpr (ImmIdentifier "anf_3")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

(*
   fun fack1 k n m = k (m * n)
   fun fack n k =
   if n <= 1 then k 1
   else fack (n-1) (fack1 k n)
   fun id x = x
   fun fac n = fack n id

   -> anf  ->

   fun fack1 k n m =
   let anf_1 = m n in
   let anf_2 = k anf_1 in
   in anf_2;

   fun fack n k =
   let anf_3 = n <= 1 in
   let anf_4 = k 1 in
   let anf_5 = n - 1 in
   let anf_6 = fack anf_5 in
   let anf_7 = fack1 k in
   let anf_8 = anf_7 n in
   let anf_9 =anf_6 anf_8 in
   if anf_3 then anf_4 else anf_9;

   fun ll_1 x = x;

   fun n =
   let anf_10 = fack n in
   let anf_11 = anf_10 ll_1
   in anf_11
*)
let%test _ =
  let ll =
    [ LFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , LApp (LIdentifier "k", LBinaryOp (Mult, LIdentifier "m", LIdentifier "n")) )
    ; LFun
        ( "fack"
        , [ "n"; "k" ]
        , LIfThenElse
            ( LBinaryOp (LtOrEq, LIdentifier "n", LLiteral (LInt 1))
            , LApp (LIdentifier "k", LLiteral (LInt 1))
            , LApp
                ( LApp
                    ( LIdentifier "fack"
                    , LBinaryOp (Sub, LIdentifier "n", LLiteral (LInt 1)) )
                , LApp (LApp (LIdentifier "fack1", LIdentifier "k"), LIdentifier "n") ) )
        )
    ; LFun ("ll_1", [ "x" ], LIdentifier "x")
    ; LFun
        ( "fac"
        , [ "n" ]
        , LApp (LApp (LIdentifier "fack", LIdentifier "n"), LIdentifier "ll_1") )
    ]
  in
  let anf =
    [ AFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , ALet
            ( "anf_1"
            , CBinaryOp (Mult, ImmIdentifier "m", ImmIdentifier "n")
            , ALet
                ( "anf_2"
                , CApp (ImmIdentifier "k", ImmIdentifier "anf_1")
                , ACExpr (CImmExpr (ImmIdentifier "anf_2")) ) ) )
    ; AFun
        ( "fack"
        , [ "n"; "k" ]
        , ALet
            ( "anf_3"
            , CBinaryOp (LtOrEq, ImmIdentifier "n", ImmInt 1)
            , ALet
                ( "anf_4"
                , CApp (ImmIdentifier "k", ImmInt 1)
                , ALet
                    ( "anf_5"
                    , CBinaryOp (Sub, ImmIdentifier "n", ImmInt 1)
                    , ALet
                        ( "anf_6"
                        , CApp (ImmIdentifier "fack", ImmIdentifier "anf_5")
                        , ALet
                            ( "anf_7"
                            , CApp (ImmIdentifier "fack1", ImmIdentifier "k")
                            , ALet
                                ( "anf_8"
                                , CApp (ImmIdentifier "anf_7", ImmIdentifier "n")
                                , ALet
                                    ( "anf_9"
                                    , CApp (ImmIdentifier "anf_6", ImmIdentifier "anf_8")
                                    , ACExpr
                                        (CIfThenElse
                                           ( ImmIdentifier "anf_3"
                                           , ImmIdentifier "anf_4"
                                           , ImmIdentifier "anf_9" )) ) ) ) ) ) ) ) )
    ; AFun ("ll_1", [ "x" ], ACExpr (CImmExpr (ImmIdentifier "x")))
    ; AFun
        ( "fac"
        , [ "n" ]
        , ALet
            ( "anf_10"
            , CApp (ImmIdentifier "fack", ImmIdentifier "n")
            , ALet
                ( "anf_11"
                , CApp (ImmIdentifier "anf_10", ImmIdentifier "ll_1")
                , ACExpr (CImmExpr (ImmIdentifier "anf_11")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

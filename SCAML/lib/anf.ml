(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RestrictedAst
open Llast
open Ast
open AnfPrinter
open Counter

(** Variable Generation *)
let gen_varname base id = Format.sprintf "%d_%s" id base

(** Converts constant to immediate value *)
let conv_const = function
  | CBool b -> ImmBool b
  | CInt i -> ImmNum i
  | CUnit -> ImmUnit
;;

(** Converts pattern to immediate pattern *)
let conv_pattern = function
  | PWild -> PImmWild
  | PVar id -> PImmExpr (ImmId id)
  | PConst c -> PImmExpr (conv_const c)
;;

open IState
open IState.Syntax
let rec anf_expr (e : llexpr) (expr_with_imm_hole : immexpr -> aexpr t) : aexpr t =
  match e with
  | LLConst c -> expr_with_imm_hole @@ conv_const c
  | LLVar id -> expr_with_imm_hole @@ ImmId id
  | LLBinOp (op, left, right) ->
    anf_expr left (fun limm ->
      anf_expr right (fun rimm ->
        fresh_name_int
        >>= fun id ->
        let var = gen_varname "b_op" id in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CBinOp (op, limm, rimm), ae))))
  | LLIf (cond, t, e) ->
    anf_expr cond (fun cimm ->
      anf_expr t (fun timm ->
        anf_expr e (fun eimm ->
          expr_with_imm_hole cimm
          >>= fun aec ->
          expr_with_imm_hole timm
          >>= fun aet ->
          expr_with_imm_hole eimm >>= fun aee -> return (AIf (aec, aet, aee)))))
  | LLApp (f, arg) ->
    anf_expr f (fun fimm ->
      anf_expr arg (fun argimm ->
        fresh_name_int
        >>= fun id ->
        let var = gen_varname "app" id in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CApp (fimm, argimm), ae))))
  | LLLetIn (varname, e1, e2) ->
    anf_expr e1 (fun immval ->
      anf_expr e2 expr_with_imm_hole
      >>= fun body -> return (ALetIn (varname, CImmExpr immval, body)))
;;

let anf_binding = function
  | LLLet (r, varname, args, e) ->
    anf_expr e (fun ie -> return (ACExpr (CImmExpr ie)))
    >>= fun anf_e -> return (ALet (r, varname, List.map conv_pattern args, anf_e))
;;

let anf_program (binds : llbinding list) =
  List.map (fun bind -> snd @@ IState.runState ~init:0 (anf_binding bind)) binds
;;

let print_anf_prog llbinds =
  let res = anf_program llbinds in
  Format.printf "%a" pp_prexpr res
;;

let print_anf_expr llexpr =
  let res =
    snd
    @@ IState.runState ~init:0 (anf_expr llexpr (fun ie -> return (ACExpr (CImmExpr ie))))
  in
  Format.printf "%a" pp_aexpr res
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp (Sub, LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4)), LLConst (CInt 2));
  [%expect {|
    let 0_b_op = 5 + 4 in
     let 1_b_op = 0_b_op - 2 in
     1_b_op |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLBinOp (Sub, LLConst (CInt 4), LLConst (CInt 3)))
       , LLConst (CInt 2) );
  [%expect
    {|
    let 0_b_op = 4 - 3 in
     let 1_b_op = 5 + 0_b_op in
     let 2_b_op = 1_b_op + 2 in
     2_b_op |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4))
       , LLBinOp (Add, LLConst (CInt 3), LLConst (CInt 2)) );
  [%expect
    {|
    let 0_b_op = 5 + 4 in
     let 1_b_op = 3 + 2 in
     let 2_b_op = 0_b_op + 1_b_op in
     2_b_op |}]
;;

let%expect_test _ =
  (*
     let fack1 k n m = k (n * m);;
     let rec fack n k = if n <= 1 then k 1 else fack (n-1) (fack1 k n);;
     let id x = x;;
     let fac n = fack n id
  *)
  print_anf_prog
  @@ [ LLLet
         ( false
         , "fack1"
         , [ PVar "k"; PVar "n"; PVar "m" ]
         , LLApp (LLVar "k", LLBinOp (Mul, LLVar "n", LLVar "m")) )
     ; LLLet
         ( true
         , "fack"
         , [ PVar "n"; PVar "k" ]
         , LLIf
             ( LLBinOp (Leq, LLVar "n", LLConst (CInt 1))
             , LLApp (LLVar "k", LLConst (CInt 1))
             , LLApp
                 ( LLApp (LLVar "fack", LLBinOp (Sub, LLVar "n", LLConst (CInt 1)))
                 , LLApp (LLApp (LLVar "fack1", LLVar "k"), LLVar "n") ) ) )
     ; LLLet (false, "id", [ PVar "x" ], LLVar "x")
     ; LLLet
         (false, "fac", [ PVar "n" ], LLApp (LLApp (LLVar "fack", LLVar "n"), LLVar "id"))
     ];
  [%expect
    {|
    let fack1 k n m = let 0_b_op = n * m in
     let 1_app = k 0_b_op in
     1_app;;
    let rec fack n k = let 0_b_op = n <= 1 in
     let 1_app = k 1 in
     let 2_b_op = n - 1 in
     let 3_app = fack 2_b_op in
     let 4_app = fack1 k in
     let 5_app = 4_app n in
     let 6_app = 3_app 5_app in
     if 0_b_op then 1_app else 6_app;;
    let id x = x;;
    let fac n = let 0_app = fack n in
     let 1_app = 0_app id in
     1_app |}]
;;

let%expect_test _ =
  (*
     let id x = x
     let acc1 acc x y = acc (x + y)
     let acc2 fib_func n acc x = fib_func (n - 2) (acc1 acc x)
     let rec fibo_cps n acc = if n < 3 then acc 1 else fibo_cps (n - 1) (acc2 fibo_cps n acc)
     let fibo n = fibo_cps n id
  *)
  print_anf_prog
  @@ [ LLLet (false, "id", [ PVar "x" ], LLVar "x")
     ; LLLet
         ( false
         , "acc1"
         , [ PVar "acc"; PVar "x"; PVar "y" ]
         , LLApp (LLVar "acc", LLBinOp (Add, LLVar "x", LLVar "y")) )
     ; LLLet
         ( false
         , "acc2"
         , [ PVar "fib_func"; PVar "n"; PVar "acc"; PVar "x" ]
         , LLApp
             ( LLApp (LLVar "fib_func", LLBinOp (Sub, LLVar "n", LLConst (CInt 2)))
             , LLApp (LLApp (LLVar "acc1", LLVar "acc"), LLVar "x") ) )
     ; LLLet
         ( true
         , "fibo_cps"
         , [ PVar "n"; PVar "acc" ]
         , LLIf
             ( LLBinOp (Less, LLVar "n", LLConst (CInt 3))
             , LLApp (LLVar "acc", LLConst (CInt 1))
             , LLApp
                 ( LLApp (LLVar "fibo_cps", LLBinOp (Sub, LLVar "n", LLConst (CInt 1)))
                 , LLApp
                     ( LLApp (LLApp (LLVar "acc2", LLVar "fibo_cps"), LLVar "n")
                     , LLVar "acc" ) ) ) )
     ; LLLet
         ( false
         , "fibo"
         , [ PVar "n" ]
         , LLApp (LLApp (LLVar "fibo_cps", LLVar "n"), LLVar "id") )
     ];
  [%expect
    {|
    let id x = x;;
    let acc1 acc x y = let 0_b_op = x + y in
     let 1_app = acc 0_b_op in
     1_app;;
    let acc2 fib_func n acc x = let 0_b_op = n - 2 in
     let 1_app = fib_func 0_b_op in
     let 2_app = acc1 acc in
     let 3_app = 2_app x in
     let 4_app = 1_app 3_app in
     4_app;;
    let rec fibo_cps n acc = let 0_b_op = n < 3 in
     let 1_app = acc 1 in
     let 2_b_op = n - 1 in
     let 3_app = fibo_cps 2_b_op in
     let 4_app = acc2 fibo_cps in
     let 5_app = 4_app n in
     let 6_app = 5_app acc in
     let 7_app = 3_app 6_app in
     if 0_b_op then 1_app else 7_app;;
    let fibo n = let 0_app = fibo_cps n in
     let 1_app = 0_app id in
     1_app |}]
;;

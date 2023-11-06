(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RestrictedAst
open Llast
open Ast
open AnfPrinter

(** Counter *)
let count = ref 0

(** Variable Generation *)
let gen_var base =
  let var = Format.sprintf "%d_%s" !count base in
  incr count;
  var
;;

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

let rec anf_expr (e : llexpr) (expr_with_imm_hole : immexpr -> aexpr) : aexpr =
  match e with
  | LLConst c -> expr_with_imm_hole @@ conv_const c
  | LLVar id -> expr_with_imm_hole @@ ImmId id
  | LLBinOp (op, left, right) ->
    let varname = gen_var "b_op" in
    anf_expr left (fun limm ->
      anf_expr right (fun rimm ->
        ALetIn (varname, CBinOp (op, limm, rimm), expr_with_imm_hole @@ ImmId varname)))
  | LLIf (cond, t, e) ->
    anf_expr cond (fun cimm ->
      anf_expr t (fun timm ->
        anf_expr e (fun eimm ->
          AIf (expr_with_imm_hole cimm, expr_with_imm_hole timm, expr_with_imm_hole eimm))))
  | LLApp (f, arg) ->
    let varname = gen_var "app" in
    anf_expr f (fun fimm ->
      anf_expr arg (fun argimm ->
        ALetIn (varname, CApp (fimm, argimm), expr_with_imm_hole @@ ImmId varname)))
  | LLLetIn (varname, e1, e2) ->
    anf_expr e1 (fun immval ->
      ALetIn (varname, CImmExpr immval, anf_expr e2 expr_with_imm_hole))
;;

let anf_binding = function
  | LLLet (r, varname, args, e) ->
    let anf_e = anf_expr e (fun ie -> ACExpr (CImmExpr ie)) in
    ALet (r, varname, List.map conv_pattern args, anf_e)
;;

let anf_program (binds : llbinding list) = List.map anf_binding binds

let print_anf_prog llbinds =
  let res = anf_program llbinds in
  Format.printf "%a" pp_prexpr res
;;

let print_anf_bind llbind =
  let res = anf_binding llbind in
  Format.printf "%a" pp_bexpr res
;;

let print_anf_expr llexpr =
  let res = anf_expr llexpr (fun ie -> ACExpr (CImmExpr ie)) in
  Format.printf "%a" pp_aexpr res
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp (Sub, LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4)), LLConst (CInt 2));
  [%expect {|
    let 1_b_op = 5 + 4 in
     let 0_b_op = 1_b_op - 2 in
     0_b_op |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLBinOp (Sub, LLConst (CInt 4), LLConst (CInt 3)))
       , LLConst (CInt 2) );
  [%expect
    {|
    let 4_b_op = 4 - 3 in
     let 3_b_op = 5 + 4_b_op in
     let 2_b_op = 3_b_op + 2 in
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
    let 6_b_op = 5 + 4 in
     let 7_b_op = 3 + 2 in
     let 5_b_op = 6_b_op + 7_b_op in
     5_b_op |}]
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
    let fack1 k n m = let 9_b_op = n * m in
     let 8_app = k 9_b_op in
     8_app;;
    let rec fack n k = let 10_b_op = n <= 1 in
     let 11_app = k 1 in
     let 14_b_op = n - 1 in
     let 13_app = fack 14_b_op in
     let 16_app = fack1 k in
     let 15_app = 16_app n in
     let 12_app = 13_app 15_app in
     if 10_b_op then 11_app else 12_app;;
    let id x = x;;
    let fac n = let 18_app = fack n in
     let 17_app = 18_app id in
     17_app |}]
;;

let%expect_test _ =
  (*
     let id x = x
     let sum x y = x + y
     let cont_maker helper_ctx n x = helper_ctx (n - 2) (sum x)

     let rec helper n cont =
     if n < 3 then cont 1 else cont (helper (n - 1) (cont_maker helper n))
     ;;

     let fib_cps n = helper n id
  *)
  print_anf_prog
  @@ [ LLLet (false, "id", [ PVar "x" ], LLVar "x")
     ; LLLet (false, "sum", [ PVar "x"; PVar "y" ], LLBinOp (Add, LLVar "x", LLVar "y"))
     ; LLLet
         ( false
         , "cont_maker"
         , [ PVar "helper_ctx"; PVar "n"; PVar "x" ]
         , LLApp
             ( LLApp (LLVar "helper_ctx", LLBinOp (Sub, LLVar "n", LLConst (CInt 2)))
             , LLApp (LLVar "sum", LLVar "x") ) )
     ; LLLet
         ( true
         , "helper"
         , [ PVar "n"; PVar "cont" ]
         , LLIf
             ( LLBinOp (Less, LLVar "n", LLConst (CInt 3))
             , LLApp (LLVar "cont", LLConst (CInt 1))
             , LLApp
                 ( LLVar "cont"
                 , LLApp
                     ( LLApp (LLVar "helper", LLBinOp (Sub, LLVar "n", LLConst (CInt 1)))
                     , LLApp (LLApp (LLVar "cont_maker", LLVar "helper"), LLVar "n") ) )
             ) )
     ; LLLet
         ( false
         , "fib_cps"
         , [ PVar "n" ]
         , LLApp (LLApp (LLVar "helper", LLVar "n"), LLVar "id") )
     ];
  [%expect
    {|
    let id x = x;;
    let sum x y = let 19_b_op = x + y in
     19_b_op;;
    let cont_maker helper_ctx n x = let 22_b_op = n - 2 in
     let 21_app = helper_ctx 22_b_op in
     let 23_app = sum x in
     let 20_app = 21_app 23_app in
     20_app;;
    let rec helper n cont = let 24_b_op = n < 3 in
     let 25_app = cont 1 in
     let 29_b_op = n - 1 in
     let 28_app = helper 29_b_op in
     let 31_app = cont_maker helper in
     let 30_app = 31_app n in
     let 27_app = 28_app 30_app in
     let 26_app = cont 27_app in
     if 24_b_op then 25_app else 26_app;;
    let fib_cps n = let 33_app = helper n in
     let 32_app = 33_app id in
     32_app |}]
;;

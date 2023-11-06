(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RestrictedAst
open Llast
open Ast

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
    let varname = gen_var "result_of_bin_op" in
    anf_expr left (fun limm ->
      anf_expr right (fun rimm ->
        ALetIn (varname, CBinOp (op, limm, rimm), expr_with_imm_hole @@ ImmId varname)))
  | LLIf (cond, t, e) ->
    anf_expr cond (fun cimm ->
      anf_expr t (fun timm ->
        anf_expr e (fun eimm ->
          AIf (expr_with_imm_hole cimm, expr_with_imm_hole timm, expr_with_imm_hole eimm))))
  | LLApp (f, arg) ->
    let varname = gen_var "result_of_app" in
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
  Format.printf "%s" (show_prexpr res)
;;

let print_anf_bind llbind =
  let res = anf_binding llbind in
  Format.printf "%s" (show_bexpr res)
;;

let print_anf_expr llexpr =
  let res = anf_expr llexpr (fun ie -> ACExpr (CImmExpr ie)) in
  Format.printf "%s" (show_aexpr res)
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp (Sub, LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4)), LLConst (CInt 2));
  [%expect
    {|
    (ALetIn ("1_result_of_bin_op", (CBinOp (Add, (ImmNum 5), (ImmNum 4))),
       (ALetIn ("0_result_of_bin_op",
          (CBinOp (Sub, (ImmId "1_result_of_bin_op"), (ImmNum 2))),
          (ACExpr (CImmExpr (ImmId "0_result_of_bin_op")))))
       )) |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLBinOp (Sub, LLConst (CInt 4), LLConst (CInt 3)))
       , LLConst (CInt 2) );
  [%expect
    {|
    (ALetIn ("4_result_of_bin_op", (CBinOp (Sub, (ImmNum 4), (ImmNum 3))),
       (ALetIn ("3_result_of_bin_op",
          (CBinOp (Add, (ImmNum 5), (ImmId "4_result_of_bin_op"))),
          (ALetIn ("2_result_of_bin_op",
             (CBinOp (Add, (ImmId "3_result_of_bin_op"), (ImmNum 2))),
             (ACExpr (CImmExpr (ImmId "2_result_of_bin_op")))))
          ))
       )) |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4))
       , LLBinOp (Add, LLConst (CInt 3), LLConst (CInt 2)) );
  [%expect
    {|
    (ALetIn ("6_result_of_bin_op", (CBinOp (Add, (ImmNum 5), (ImmNum 4))),
       (ALetIn ("7_result_of_bin_op", (CBinOp (Add, (ImmNum 3), (ImmNum 2))),
          (ALetIn ("5_result_of_bin_op",
             (CBinOp (Add, (ImmId "6_result_of_bin_op"),
                (ImmId "7_result_of_bin_op"))),
             (ACExpr (CImmExpr (ImmId "5_result_of_bin_op")))))
          ))
       )) |}]
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
    [(ALet (false, "fack1",
        [(PImmExpr (ImmId "k")); (PImmExpr (ImmId "n")); (PImmExpr (ImmId "m"))],
        (ALetIn ("9_result_of_bin_op", (CBinOp (Mul, (ImmId "n"), (ImmId "m"))),
           (ALetIn ("8_result_of_app",
              (CApp ((ImmId "k"), (ImmId "9_result_of_bin_op"))),
              (ACExpr (CImmExpr (ImmId "8_result_of_app")))))
           ))
        ));
      (ALet (true, "fack", [(PImmExpr (ImmId "n")); (PImmExpr (ImmId "k"))],
         (ALetIn ("10_result_of_bin_op", (CBinOp (Leq, (ImmId "n"), (ImmNum 1))),
            (ALetIn ("11_result_of_app", (CApp ((ImmId "k"), (ImmNum 1))),
               (ALetIn ("14_result_of_bin_op",
                  (CBinOp (Sub, (ImmId "n"), (ImmNum 1))),
                  (ALetIn ("13_result_of_app",
                     (CApp ((ImmId "fack"), (ImmId "14_result_of_bin_op"))),
                     (ALetIn ("16_result_of_app",
                        (CApp ((ImmId "fack1"), (ImmId "k"))),
                        (ALetIn ("15_result_of_app",
                           (CApp ((ImmId "16_result_of_app"), (ImmId "n"))),
                           (ALetIn ("12_result_of_app",
                              (CApp ((ImmId "13_result_of_app"),
                                 (ImmId "15_result_of_app"))),
                              (AIf (
                                 (ACExpr (CImmExpr (ImmId "10_result_of_bin_op"))),
                                 (ACExpr (CImmExpr (ImmId "11_result_of_app"))),
                                 (ACExpr (CImmExpr (ImmId "12_result_of_app")))))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            ))
         ));
      (ALet (false, "id", [(PImmExpr (ImmId "x"))],
         (ACExpr (CImmExpr (ImmId "x")))));
      (ALet (false, "fac", [(PImmExpr (ImmId "n"))],
         (ALetIn ("18_result_of_app", (CApp ((ImmId "fack"), (ImmId "n"))),
            (ALetIn ("17_result_of_app",
               (CApp ((ImmId "18_result_of_app"), (ImmId "id"))),
               (ACExpr (CImmExpr (ImmId "17_result_of_app")))))
            ))
         ))
      ] |}]
;;

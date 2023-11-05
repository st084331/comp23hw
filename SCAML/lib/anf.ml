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

(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Var_counter

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string
[@@deriving show { with_path = false }]

type cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of un_op * immexpr
  | CBinaryOp of bin_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIfThenElse of immexpr * immexpr * immexpr
[@@deriving show { with_path = false }]

type aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type abinding =
  | AVal of string * aexpr
  | AFun of string * string list * aexpr
[@@deriving show { with_path = false }]

let fresh_var = fresh_var "anf"

let rec anf (e : exp) (expr_with_hole : immexpr -> aexpr) : aexpr =
  match e with
  | EConst (CInt n) -> expr_with_hole (ImmInt n)
  | EConst (CBool b) -> expr_with_hole (ImmBool b)
  | EVar x -> expr_with_hole (ImmIdentifier x)
  | EUnOp (op, exp) ->
    anf exp (fun imm ->
      let varname = fresh_var () in
      ALet (varname, CUnaryOp (op, imm), expr_with_hole (ImmIdentifier varname)))
  | EBinOp (op, left, right) ->
    anf left (fun limm ->
      anf right (fun rimm ->
        let varname = fresh_var () in
        ALet (varname, CBinaryOp (op, limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | EApp (e1, e2) ->
    anf e1 (fun e1imm ->
      anf e2 (fun e2imm ->
        let varname = fresh_var () in
        ALet (varname, CApp (e1imm, e2imm), expr_with_hole (ImmIdentifier varname))))
  | EIf (cond, e1, e2) ->
    anf cond (fun condimm ->
      anf e1 (fun e1imm ->
        anf e2 (fun e2imm ->
          let varname = fresh_var () in
          ALet
            ( varname
            , CIfThenElse (condimm, e1imm, e2imm)
            , expr_with_hole (ImmIdentifier varname) ))))
  | ELet (bindings, body) -> anf_let_bindings bindings body expr_with_hole
  | EFun (pt, body) -> anf_fun pt body expr_with_hole

(* Implementing the handling of let bindings *)
and anf_let_bindings bindings body expr_with_hole =
  match bindings with
  | [] -> anf body expr_with_hole (* No bindings left, transform the body *)
  | (is_rec, pt, exp) :: rest ->
    anf exp (fun immexpr ->
      let varname = fresh_var () in
      match pt with
      | PtWild -> anf_let_bindings rest body expr_with_hole
      | PtVar id ->
        (* Use the original variable name for the binding *)
        let new_body = substitute id id (ELet (rest, body)) in
        ALet (id, CImmExpr immexpr, anf_let_bindings rest new_body expr_with_hole)
      | PtConst const -> anf_let_bindings rest body expr_with_hole)

(* Convert a constant to an immexpr *)
and const_to_immexpr const =
  match const with
  | CInt n -> ImmInt n
  | CBool b -> ImmBool b

(* Substitute occurrences of `old_id` with `new_id` in the expression *)
and substitute old_id new_id expr =
  match expr with
  | EConst _ -> expr
  | EVar id -> if id = old_id then EVar new_id else expr
  | EUnOp (op, e) -> EUnOp (op, substitute old_id new_id e)
  | EBinOp (op, e1, e2) ->
    EBinOp (op, substitute old_id new_id e1, substitute old_id new_id e2)
  | EApp (e1, e2) -> EApp (substitute old_id new_id e1, substitute old_id new_id e2)
  | EIf (cond, e1, e2) ->
    EIf
      ( substitute old_id new_id cond
      , substitute old_id new_id e1
      , substitute old_id new_id e2 )
  | ELet (bindings, e) ->
    ELet
      (List.map (substitute_in_binding old_id new_id) bindings, substitute old_id new_id e)
  | EFun (pt, e) ->
    (match pt with
     | PtVar id when id = old_id ->
       EFun (pt, e) (* Do not substitute shadowed variables *)
     | _ -> EFun (pt, substitute old_id new_id e))

(* Substitute in bindings *)
and substitute_in_binding old_id new_id (is_rec, pt, exp) =
  match pt with
  | PtVar id when id = old_id && not is_rec ->
    is_rec, pt, exp (* Do not substitute shadowed variables in non-recursive case *)
  | _ -> is_rec, pt, substitute old_id new_id exp

and anf_fun pt body expr_with_hole =
  let varname = fresh_var () in
  match pt with
  | PtVar arg_id ->
    (* Handling function with a single argument *)
    let anf_body =
      anf (substitute arg_id varname body) (fun imm -> ACExpr (CImmExpr imm))
    in
    ALet (varname, CImmExpr (ImmIdentifier arg_id), anf_body)
  | PtWild ->
    (* Handling function with a wildcard argument *)
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    ALet (varname, CImmExpr (ImmIdentifier "_"), anf_body)
  | PtConst const ->
    (* Handling function with a constant pattern *)
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    let check_const =
      match const with
      | CInt n ->
        CIfThenElse (ImmInt n, ImmIdentifier varname, ImmInt 0)
        (* 0 could represent false or null *)
      | CBool b -> CIfThenElse (ImmBool b, ImmIdentifier varname, ImmBool false)
    in
    ALet (varname, check_const, anf_body)
;;

let anf_program (program : prog) : abinding list =
  reset_counter ();
  List.fold_right
    (fun decl acc ->
      match decl with
      | DLet (is_rec, pt, exp) ->
        (match pt with
         | PtWild -> acc
         | PtVar id ->
           (* For a variable pattern, transform the expression and create a binding *)
           let anf_exp = anf exp (fun imm -> ACExpr (CImmExpr imm)) in
           AVal (id, anf_exp) :: acc
         | PtConst const ->
           (* Handling constant patterns in top-level let declarations *)
           let anf_exp = anf exp (fun imm -> ACExpr (CImmExpr imm)) in
           let const_immexpr = const_to_immexpr const in
           let varname = fresh_var () in
           AVal (varname, ALet (varname, CImmExpr const_immexpr, anf_exp)) :: acc))
    program
    []
;;

(* Debugging function to print an expression *)
let debug_print_expr expr = Printf.printf "Debug Expression: %s\n" (show_exp expr)

(* Debugging function to print an aexpr *)
let debug_print_aexpr aexpr =
  Printf.printf "Debug ANF Expression: %s\n" (show_aexpr aexpr)
;;

(* Inline test for ANF transformation of a simple expression *)
let%test "anf_simple_expression" =
  let expr = EConst (CInt 42) in
  debug_print_expr expr;
  let expected = ACExpr (CImmExpr (ImmInt 42)) in
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  debug_print_aexpr result;
  result = expected
;;

(* Inline test for ANF transformation of a unary operation *)
let%test "anf_unary_operation" =
  let expr = EUnOp (Minus, EConst (CInt 42)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = -42 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet (varname, CUnaryOp (Minus, ImmInt 42), ACExpr (CImmExpr (ImmIdentifier vn)))
    when varname = vn -> true
  | _ -> false
;;

(* Inline test for ANF transformation of a binary operation *)
let%test "anf_binary_operation" =
  let expr = EBinOp (Add, EConst (CInt 40), EConst (CInt 2)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = 40 + 2 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet (_, CBinaryOp (Add, ImmInt 40, ImmInt 2), ACExpr (CImmExpr (ImmIdentifier _))) ->
    true
  | _ -> false
;;

(* Inline test for ANF transformation of a conditional expression *)
let%test "anf_conditional_expression" =
  let expr = EIf (EConst (CBool true), EConst (CInt 1), EConst (CInt 0)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = if true then 1 else 0 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet
      ( _
      , CIfThenElse (ImmBool true, ImmInt 1, ImmInt 0)
      , ACExpr (CImmExpr (ImmIdentifier _)) ) -> true
  | _ -> false
;;

(* Inline test for ANF transformation of a let-in expression *)
let%test "anf_let_in_expression" =
  let expr = ELet ([ true, PtVar "x", EConst (CInt 42) ], EVar "x") in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let x = 42 in x *)
  debug_print_aexpr result;
  match result with
  | ALet ("x", CImmExpr (ImmInt 42), ACExpr (CImmExpr (ImmIdentifier "x"))) -> true
  | _ -> false
;;

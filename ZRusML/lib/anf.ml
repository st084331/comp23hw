(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string

type aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

and cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of un_op * immexpr
  | CBinaryOp of bin_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIf of immexpr * aexpr * aexpr

type pexpr =
  | PImmExpr of immexpr
  | PImmWild

type abinding = ABind of string * pexpr list * aexpr

let fresh_var_temp count () =
  incr count;
  Printf.sprintf "%s_%d" "anf" !count
;;

let new_fresh_var () =
  let count = ref 0 in
  fresh_var_temp count
;;

let rec anf_func (fresh_var : unit -> id) (e : exp) (expr_with_hole : immexpr -> aexpr)
  : aexpr
  =
  let anf = anf_func fresh_var in
  let rec anf_let_bindings bindings body expr_with_hole =
    match bindings with
    | [] -> anf body expr_with_hole
    | (_, pt, exp) :: rest ->
      anf exp (fun immexpr ->
        match pt with
        | PtWild -> anf_let_bindings rest body expr_with_hole
        | PtVar id ->
          ALet (id, CImmExpr immexpr, anf_let_bindings rest body expr_with_hole)
        | PtConst _ -> anf_let_bindings rest body expr_with_hole)
  in
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
      let new_e1 = anf e1 (fun e1imm -> ACExpr (CImmExpr e1imm)) in
      let new_e2 = anf e2 (fun e2imm -> ACExpr (CImmExpr e2imm)) in
      let varname = fresh_var () in
      ALet (varname, CIf (condimm, new_e1, new_e2), expr_with_hole (ImmIdentifier varname)))
  | ELet (bindings, body) -> anf_let_bindings bindings body expr_with_hole
  | EFun (_, body) ->
    let varname = fresh_var () in
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    ALet (varname, CImmExpr (ImmIdentifier "_"), anf_body)

and const_to_immexpr = function
  | CInt n -> ImmInt n
  | CBool b -> ImmBool b

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
     | PtVar id when id = old_id -> EFun (pt, e)
     | _ -> EFun (pt, substitute old_id new_id e))

and substitute_in_binding old_id new_id (is_rec, pt, exp) =
  match pt with
  | PtVar id when id = old_id && not is_rec -> is_rec, pt, exp
  | _ -> is_rec, pt, substitute old_id new_id exp
;;

let anf_program (program : prog) : abinding list =
  let fresh_var = new_fresh_var () in
  List.fold_right
    (fun decl acc ->
      match decl with
      | DLet (_, pt, exp) ->
        let rec get_new_exp lst = function
          | EFun (PtVar id, nxt) -> get_new_exp (PImmExpr (ImmIdentifier id) :: lst) nxt
          | EFun (PtWild, nxt) -> get_new_exp (PImmWild :: lst) nxt
          | EFun (PtConst t, nxt) ->
            get_new_exp (PImmExpr (const_to_immexpr t) :: lst) nxt
          | result -> lst, result
        in
        let lst, exp = get_new_exp [] exp in
        let lst = List.rev lst in
        let anf_exp = anf_func fresh_var exp (fun imm -> ACExpr (CImmExpr imm)) in
        let id =
          match pt with
          | PtVar id -> id
          | PtWild -> fresh_var ()
          | PtConst _ -> fresh_var ()
        in
        ABind (id, lst, anf_exp) :: acc)
    program
    []
;;

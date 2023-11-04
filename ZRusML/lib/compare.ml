(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

(* Define a comparison function for expressions *)
let rec compare_exp e1 e2 =
  match e1, e2 with
  | EConst c1, EConst c2 -> c1 = c2
  | EVar v1, EVar v2 -> v1 = v2
  | EUnOp (op1, exp1), EUnOp (op2, exp2) -> op1 = op2 && compare_exp exp1 exp2
  | EBinOp (op1, exp1a, exp1b), EBinOp (op2, exp2a, exp2b) ->
    op1 = op2 && compare_exp exp1a exp2a && compare_exp exp1b exp2b
  | EIf (exp1a, exp1b, exp1c), EIf (exp2a, exp2b, exp2c) ->
    compare_exp exp1a exp2a && compare_exp exp1b exp2b && compare_exp exp1c exp2c
  | ELet (binds1, exp1), ELet (binds2, exp2) ->
    compare_bindings binds1 binds2 && compare_exp exp1 exp2
  | EFun (pt1, exp1), EFun (pt2, exp2) -> compare_pt pt1 pt2 && compare_exp exp1 exp2
  | EApp (exp1a, exp1b), EApp (exp2a, exp2b) ->
    compare_exp exp1a exp2a && compare_exp exp1b exp2b
  | _, _ -> false

(* Define a comparison function for patterns *)
and compare_pt pt1 pt2 =
  match pt1, pt2 with
  | PtWild, PtWild -> true
  | PtVar v1, PtVar v2 -> v1 = v2
  | PtConst c1, PtConst c2 -> c1 = c2
  | _, _ -> false

(* Define a comparison function for bindings *)
and compare_bindings b1 b2 =
  List.length b1 = List.length b2
  && List.for_all2
       (fun (r1, p1, e1) (r2, p2, e2) -> r1 = r2 && compare_pt p1 p2 && compare_exp e1 e2)
       b1
       b2
;;

let print_bin_op op =
  match op with
  | And -> "And"
  | Or -> "Or"
  | Less -> "Less"
  | Leq -> "Leq"
  | Gre -> "Gre"
  | Geq -> "Geq"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

and print_un_op op =
  match op with
  | Not -> "Not"
  | Minus -> "Minus"
;;

let rec print_pt pt =
  match pt with
  | PtWild -> "_"
  | PtVar v -> v
  | PtConst c -> print_const c

and print_const c =
  match c with
  | CInt i -> Printf.sprintf "CInt(%d)" i
  | CBool b -> Printf.sprintf "CBool(%b)" b

and print_exp exp =
  match exp with
  | EConst c -> print_const c
  | EVar v -> Printf.sprintf "EVar(%s)" v
  | EUnOp (op, e) -> Printf.sprintf "EUnOp(%s, %s)" (print_un_op op) (print_exp e)
  | EBinOp (op, e1, e2) ->
    Printf.sprintf "EBinOp(%s, %s, %s)" (print_bin_op op) (print_exp e1) (print_exp e2)
  | EIf (e1, e2, e3) ->
    Printf.sprintf "EIf(%s, %s, %s)" (print_exp e1) (print_exp e2) (print_exp e3)
  | ELet (bindings, e) ->
    Printf.sprintf "ELet(%s, %s)" (print_bindings bindings) (print_exp e)
  | EFun (pt, e) -> Printf.sprintf "EFun(%s, %s)" (print_pt pt) (print_exp e)
  | EApp (e1, e2) -> Printf.sprintf "EApp(%s, %s)" (print_exp e1) (print_exp e2)

and print_bindings bindings =
  let print_binding (b, pt, e) =
    Printf.sprintf "(%b, %s, %s)" b (print_pt pt) (print_exp e)
  in
  "[" ^ String.concat "; " (List.map print_binding bindings) ^ "]"
;;

let assert_equal
  (expected : exp * (bool * pt * exp) list)
  (actual : exp * (bool * pt * exp) list)
  =
  let exp_expected, lets_expected = expected in
  let exp_actual, lets_actual = actual in
  if compare_exp exp_expected exp_actual && compare_bindings lets_expected lets_actual
  then print_endline "Test passed."
  else (
    let msg =
      Printf.sprintf
        "Expected expression:\n%s\nBut got:\n%s"
        (print_exp exp_expected)
        (print_exp exp_actual)
    in
    failwith msg)
;;

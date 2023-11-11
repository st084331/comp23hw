(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast

let string_of_unary_op = function
  | Neg -> "neg"
  | Not -> "not"

let string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "="
  | Lt -> "<"
  | LtOrEq -> "<="
  | Gt -> ">"
  | GtOrEq -> ">="
  | And -> "and"
  | Or -> "or"
;;

let string_of_immexpr = function
  | ImmInt n -> string_of_int n
  | ImmBool b -> string_of_bool b
  | ImmIdentifier s -> s
;;

let string_of_cexpr = function
  | CImmExpr ie -> string_of_immexpr ie
  | CUnaryOp (op, ie) ->
    Printf.sprintf "%s %s" (string_of_unary_op op) (string_of_immexpr ie)
  | CBinaryOp (op, ie1, ie2) ->
    Printf.sprintf
      "(%s %s %s)"
      (string_of_immexpr ie1)
      (string_of_binary_op op)
      (string_of_immexpr ie2)
  | CApp (ie1, ie2) ->
    Printf.sprintf "%s %s" (string_of_immexpr ie1) (string_of_immexpr ie2)
  | CIfThenElse (ie1, ie2, ie3) ->
    Printf.sprintf
      "if %s then %s else %s"
      (string_of_immexpr ie1)
      (string_of_immexpr ie2)
      (string_of_immexpr ie3)

let rec string_of_aexpr = function
  | ALet (id, ce, ae) ->
    Printf.sprintf "let %s = %s in\n%s" id (string_of_cexpr ce) (string_of_aexpr ae)
  | ACExpr ce -> string_of_cexpr ce

let string_of_abinding = function
  | AVal (id, ae) -> Printf.sprintf "val %s = %s\n" id (string_of_aexpr ae)
  | AFun (id, id_list, ae) ->
    Printf.sprintf "fun %s %s = \n%s\n" id (String.concat " " id_list) (string_of_aexpr ae)

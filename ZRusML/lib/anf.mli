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

val anf_program : prog -> abinding list

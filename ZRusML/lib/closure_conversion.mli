(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

(* Helper function to collect all free variables in an expression *)
val free_vars : exp -> string list -> string list

(* Closure conversion for expressions *)
val convert_exp : exp -> string list -> exp

(* Closure conversion for a program *)
val closure_convert_program : decl list -> decl list

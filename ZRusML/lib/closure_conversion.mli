(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Core

(** Set of strings *)
module StringSet : sig
  include Set.S with type Elt.t = string
end

(** Find free variables in an expression *)
val find_free_vars : exp -> StringSet.t -> StringSet.t

(** Convert an expression for closure *)
val closure_convert_exp : StringSet.t -> exp -> exp

(** Convert a program for closure *)
val closure_convert_prog : prog -> prog

(** Convert an abstract syntax tree for closure *)
val closure_convert : prog -> prog

(** Convert a pattern to an identifier *)
val pattern_to_id : pt -> id

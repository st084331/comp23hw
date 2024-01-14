(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val get_validated_name : int -> string
val validate_prog : Ast.decl list -> Ast.decl list

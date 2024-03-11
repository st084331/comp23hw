(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** Transform decls to decls without closures *)
val transform_decls : Ast.decl list -> Ast.decl list

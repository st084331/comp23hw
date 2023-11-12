(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** By formatter print pretty formatted Ast.const *)
val pp_const : Format.formatter -> Ast.const -> unit

(** By formatter print pretty formatted Ast.bin_op *)
val pp_bin_op : Format.formatter -> Ast.bin_op -> unit

(** By formatter print pretty formatted Ast.un_op *)
val pp_un_op : Format.formatter -> Ast.un_op -> unit

(** By formatter print pretty formatted Ast.pt *)
val pp_pt : Format.formatter -> Ast.pt -> unit

(** By formatter and count print count tabs *)
val print_tabs : Format.formatter -> int -> unit

(** By formatter print pretty formatted Ast.const *)
val pp_exp : Format.formatter -> int -> Ast.exp -> unit

(** By formatter print pretty formatted Ast.const *)
val pp_binding : Format.formatter -> int -> bool * Ast.pt * Ast.exp -> unit

(** By formatter print pretty formatted Ast.const *)
val pp_decl : Format.formatter -> Ast.decl -> unit

(** By formatter print pretty formatted Ast.decl list *)
val pp_prog : Format.formatter -> Ast.decl list -> unit

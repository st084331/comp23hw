(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* ANF Transformation Interface *)

open Ast

type error =
  | NotImplemented
  | OtherError of string

val fresh_var : unit -> string
val exp_to_anf : exp -> (exp * (bool * pt * exp) list, error) result
val bindings_to_anf : binding list -> (exp * (bool * pt * exp) list, error) result
val reset_counter : unit -> unit

(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* This module provides a parser for the language. *)

open Ast
open Base

(** [parse p s] applies the parser [p] on string [s]. *)
val parse : 'a Angstrom.t -> string -> 'a Angstrom.result

(** [prog] is the main parser for the entire program. *)
val prog : prog Angstrom.t

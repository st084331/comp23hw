(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* ANF Transformation Interface *)

open Ast

(* Generate a fresh variable name. *)
val fresh_var : unit -> string

(* Transform an expression into A-Normal Form (ANF),
   returning the transformed expression along with any necessary let-bindings. *)
val exp_to_anf : exp -> exp * (bool * pt * exp) list

(* Transform a list of bindings into ANF,
   returning the transformed bindings along with any necessary let-bindings. *)
val bindings_to_anf : binding list -> exp * (bool * pt * exp) list

(** Resets the counter used for generating fresh variables. *)
val reset_counter : unit -> unit

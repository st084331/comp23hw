(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module type CounterState = sig
  val counter : int ref
  val reset : unit -> unit
end

module FreshVarGenerator (State : CounterState) : sig
  val fresh_var : string -> unit -> string
  val reset : unit -> unit
end

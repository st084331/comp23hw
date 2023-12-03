(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
module type CounterState = sig
  val counter : int ref
  val reset : unit -> unit
end

(* Functor that takes a CounterState and provides fresh variable functionality *)
module FreshVarGenerator (State : CounterState) = struct
  let fresh_var name () =
    State.counter := !State.counter + 1;
    Printf.sprintf "%s_%d" name !State.counter
  ;;

  let reset = State.reset
end

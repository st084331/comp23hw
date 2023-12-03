module type CounterState = sig
  val counter : int ref
  val reset : unit -> unit
end

module FreshVarGenerator (State : CounterState) : sig
  val fresh_var : string -> unit -> string
  val reset : unit -> unit
end

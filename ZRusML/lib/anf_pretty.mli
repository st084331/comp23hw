(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val show_immexpr : Anf.immexpr -> string
val pp_immexpr : Format.formatter -> Anf.immexpr -> unit
val show_cexpr : Anf.cexpr -> string
val pp_cexpr : Format.formatter -> Anf.cexpr -> unit
val show_pexpr : Anf.pexpr -> string
val pp_pexpr : Format.formatter -> string -> unit
val show_aexpr : Anf.aexpr -> string
val pp_aexpr : Format.formatter -> Anf.aexpr -> unit
val show_abinding : Anf.abinding -> string
val pp_abinding : Format.formatter -> Anf.abinding -> unit
val show_abinding_list : Anf.abinding list -> string
val pp_abinding_list : Format.formatter -> Anf.abinding list -> unit

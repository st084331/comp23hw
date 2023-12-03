(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val create_fresh_var_system : unit -> (string -> unit -> string) * (unit -> unit)

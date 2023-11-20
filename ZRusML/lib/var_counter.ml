(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
let count = ref 0

let fresh_var name () =
  incr count;
  Printf.sprintf "%s_%d" name !count
;;

let reset_counter () = count := 0

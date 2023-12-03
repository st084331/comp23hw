(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
let create_fresh_var_system () =
  let count = ref 0 in
  let fresh_var name () =
    incr count;
    Printf.sprintf "%s_%d" name !count
  in
  let reset_counter () = count := 0 in
  fresh_var, reset_counter
;;

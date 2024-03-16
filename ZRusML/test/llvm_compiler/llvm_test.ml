(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open ZRusML_lib.Compiler

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)

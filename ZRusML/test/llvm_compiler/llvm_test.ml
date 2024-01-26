(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open ZRusML_lib.Interpreter

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)

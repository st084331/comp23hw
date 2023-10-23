(** Copyright 2023-2024, Anton Kraev and Polina Badreeva*)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () = KREML_lib.Inferencer.parse_and_inference (Stdio.In_channel.input_all stdin)

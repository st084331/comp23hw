(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let parse_to_anf program =
  let parsed_ast = KREML_lib.Parser.parse_optimistically program in
  let _ = KREML_lib.Inferencer.parse_and_infer program in
  let clos_conv = KREML_lib.Clos_conv.cc_program parsed_ast in
  let lambda_lifting = KREML_lib.Lambda_lifting.ll_program clos_conv in
  let anf_conv = KREML_lib.Anf_conv.anf_program lambda_lifting in
  let anf_string = List.map KREML_lib.Anf.show_abinding anf_conv |> String.concat "\n" in
  print_endline anf_string;
  ()
;;


let () = parse_to_anf (Stdio.In_channel.input_all stdin)

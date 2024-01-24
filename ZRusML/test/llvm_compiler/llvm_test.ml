(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open ZRusML_lib.Anf
open ZRusML_lib.Anf_pretty
open ZRusML_lib.Ast
open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer
open ZRusML_lib.Closure_conversion
open ZRusML_lib.Code_inferencer
open ZRusML_lib.Lambda_lifting
open ZRusML_lib.Ast_validator
open ZRusML_lib.Llvm_compiler
open Base
open Stdio
open Stdlib

let print_prog_result code =
  match parse prog code with
  | Ok res ->
    let prog_closure = transform_decls res in
    let lifted = lift_prog prog_closure in
    (* let _ = pp_prog Format.std_formatter lifted in *)
    let validated = validate_prog lifted in
    let anf_prog = anf_program validated in
    (match codegen_program anf_prog with
     | Ok llvalue_list ->
       Base.List.iter llvalue_list ~f:(fun f ->
         Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f))
     | Error e -> Stdlib.Format.printf "Error%s" e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)

(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Anf
open Parser
open Closure_conversion
open Lambda_lifting
open Ast_validator
open Llvm_compiler
open Code_inferencer
open Typing
open Inferencer

let print_prog_result code =
  match parse prog code with
  | Ok res ->
    let stdlib =
      [ "print_int", TArr (TGround Int, TGround Unit)
      ; "print_bool", TArr (TGround Bool, TGround Unit)
      ; "print_char", TArr (TGround Int, TGround Unit)
      ]
    in
    let stdlib_env =
      Base.List.fold_left
        ~init:TypeEnv.empty
        ~f:(fun acc (id, t) ->
          TypeEnv.extend acc id (Base.Set.empty (module Base.Int), t))
        stdlib
    in
    (match env_show_inference res stdlib_env with
     | Ok _ ->
       let prog_closure = transform_decls res in
       let lifted = lift_prog prog_closure in
       let validated = validate_prog lifted in
       let anf_prog = anf_program validated in
       (match codegen_program anf_prog with
        | Ok llvalue_list ->
          Base.List.iter llvalue_list ~f:(fun f ->
            Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f))
        | Error e -> Stdlib.Format.printf "Error\n%s" e)
     | Error res -> Stdlib.Format.printf "%s" res)
  | Error e -> Stdlib.Format.printf "Parser error! %s" e
;;

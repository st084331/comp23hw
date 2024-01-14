(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llvm
open Anf

val codegen_program : Anf.abinding list -> (Llvm.llvalue list, string) result

(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type immexpr =
  | ImmValue of string (* Variable *)
  | ImmNum of int (* Constants *)
  | ImmString of string
  | ImmBool of bool
  | ImmUnit

type cexpr =
  | CImm of immexpr (* Constant or variable *)
  | CApp of immexpr * immexpr (* Function application *)
  | CIfElse of immexpr * ablock * ablock (* If-then-else *)

and alet = string * cexpr (* Single assignment a = 3 * 2 *)
and ablock = immexpr * alet list (* ANF Block: a = 3; b = 2 + 1; c = a + b; c *)

type aprogram = ablock (* Whole program *)
type afun = string * string list * ablock (* Functions *)

val anf : expr -> afun list * aprogram

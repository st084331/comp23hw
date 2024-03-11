(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open ZRusML_lib.Typing

let%expect_test "pp_type prints int type correctly" =
  pp_typ Format.std_formatter int_typ;
  [%expect "int"]
;;

let%expect_test "pp_type prints bool type correctly" =
  pp_typ Format.std_formatter bool_typ;
  [%expect "bool"]
;;

let%expect_test "pp_type prints arrow type correctly" =
  pp_typ Format.std_formatter (arrow_t int_typ bool_typ);
  [%expect "int -> bool"]
;;

let%expect_test "pp_error prints OccursCheck correctly" =
  pp_error Format.std_formatter `OccursCheck;
  [%expect "Occurs check failed."]
;;

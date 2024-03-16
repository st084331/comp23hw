(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type type_variable_number = int
type identifier = string

type ground_type =
  | Int
  | Bool
  | Unit

type typ =
  | TVar of type_variable_number (** 'a *)
  | TArr of typ * typ (** string -> int *)
  | TGround of ground_type (** int *)

(* Ground types *)
let int_typ = TGround Int
let bool_typ = TGround Bool
let var_t n = TVar n
let arrow_t left_type right_type = TArr (left_type, right_type)

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of identifier (** Using undefined variable *)
  | `UnificationFailed of typ * typ (** Unify of typs failed *)
  | `Unreachable (** Bug in parser *)
  | `Not_function
  | `Matching_failed
  ]

let rec show_typ typ =
  let open Format in
  match typ with
  | TGround x ->
    (match x with
     | Int -> "int"
     | Bool -> "bool"
     | Unit -> "unit")
  | TArr (typ_left, typ_right) ->
    let left =
      match typ_left with
      | TArr _ -> sprintf "(%s)" (show_typ typ_left)
      | _ -> show_typ typ_left
    in
    let right =
      match typ_left, typ_right with
      | TArr _, TArr _ -> sprintf "(%s)" (show_typ typ_right)
      | _ -> show_typ typ_right
    in
    sprintf "%s -> %s" left right
  | TVar var -> sprintf "%s" @@ "'" ^ Char.escaped (Stdlib.Char.chr (var + 97))
;;

let pp_typ fmt typ = Format.fprintf fmt "%s" (show_typ typ)

let show_error (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> "Occurs check failed."
  | `NoVariable identifier ->
    sprintf "Elaboration failed: Unbound value identifier %s" identifier
  | `UnificationFailed (t1, t2) ->
    sprintf
      "Elaboration failed: Rules disagree on type: Cannot merge %s and %s"
      (show_typ t1)
      (show_typ t2)
  | `Unreachable -> "Not reachable."
  | `Not_function -> "Applying not a function"
  | `Matching_failed -> "Non-exhaustive matching"
;;

let pp_error fmt err = Format.fprintf fmt "%s" (show_error err)

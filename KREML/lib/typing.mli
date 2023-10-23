type type_variable_number = int
type identifier = string
type ground_type = Int | Bool
val pp_ground_type : Format.formatter -> ground_type -> unit
val show_ground_type : ground_type -> identifier
type typ =
    TVar of type_variable_number
  | TEqualityVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground_type
  | TUnit
val int_t : typ
val bool_t : typ
val unit_t : typ
val var_t : type_variable_number -> typ
val var_eq_t : type_variable_number -> typ
val arrow_t : typ -> typ -> typ
val tuple_t : typ list -> typ
val list_t : typ -> typ
type scheme =
    (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ
type error =
    [ `NoVariable of identifier
    | `OccursCheck
    | `UnificationFailed of typ * typ
    | `Unreachable ]
val pp_type : Format.formatter -> typ -> unit
val print_typ : typ -> unit
val pp_error : Format.formatter -> error -> unit
val print_type_error : error -> unit

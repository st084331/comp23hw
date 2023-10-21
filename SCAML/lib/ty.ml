open Format


type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TBool
  | TInt
  | TUnit
  | TVar of binder
  | TArrow of ty * ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = TArrow (l, r)
let int_typ = TInt
let bool_typ = TBool
let unit_typ = TUnit
let v x = TVar x

let rec pp_typ ppf = function
  | TVar n -> fprintf ppf "'_%d" n
  | TInt -> pp_print_string ppf "int"
  | TBool -> pp_print_string ppf "bool"
  | TArrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | TUnit -> pp_print_string ppf "unit"
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

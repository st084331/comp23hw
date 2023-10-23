type identifier = string
val pp_identifier : Format.formatter -> identifier -> unit
val show_identifier : identifier -> identifier
type literal = LInt of int | LBool of bool
val pp_literal : Format.formatter -> literal -> unit
val show_literal : literal -> identifier
type binary_op =
    Add
  | Sub
  | Mult
  | Div
  | Eq
  | Lt
  | LtOrEq
  | Gt
  | GtOrEq
  | And
  | Or
val pp_binary_op : Format.formatter -> binary_op -> unit
val show_binary_op : binary_op -> identifier
type unary_op = Neg | Not
val pp_unary_op : Format.formatter -> unary_op -> unit
val show_unary_op : unary_op -> identifier
type expr =
    ELiteral of literal
  | EIdentifier of identifier
  | EUnaryOp of unary_op * expr
  | EBinaryOp of binary_op * expr * expr
  | EApp of expr * expr
  | EAbs of identifier * expr
  | EIfThenElse of expr * expr * expr
  | ELetIn of binding list * expr
and binding =
    BVal of identifier * expr
  | BFun of identifier * identifier list * expr
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> identifier
val pp_binding : Format.formatter -> binding -> unit
val show_binding : binding -> identifier
val e_literal : literal -> expr
val e_identifier : identifier -> expr
val e_unary_op : unary_op -> expr -> expr
val e_app : expr -> expr -> expr
val e_abs : identifier -> expr -> expr
val e_if_then_else : expr -> expr -> expr -> expr
val e_binary_op : binary_op -> expr -> expr -> expr
val e_let_in : binding list -> expr -> expr
val b_val : identifier -> expr -> binding
val b_fun : identifier -> identifier list -> expr -> binding
val badd : 'a -> binary_op
val bsub : 'a -> binary_op
val bmul : 'a -> binary_op
val bdiv : 'a -> binary_op
val beq : 'a -> binary_op
val blt : 'a -> binary_op
val blte : 'a -> binary_op
val bgt : 'a -> binary_op
val bgte : 'a -> binary_op
val band : 'a -> binary_op
val bor : 'a -> binary_op
val uneg : 'a -> unary_op
val unot : 'a -> unary_op
val lint : int -> literal
val lbool : bool -> literal

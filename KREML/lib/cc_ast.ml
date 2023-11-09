open Ast

(** Simplified ast representation which is the result of closure conversion stage.
    The difference is that there's 1 binding in let_in expr and uncurried lambda functions *)

type cc_expr =
  | CLiteral of literal
  | CIdentifier of identifier
  | CUnaryOp of unary_op * cc_expr
  | CBinaryOp of binary_op * cc_expr * cc_expr
  | CApp of cc_expr * cc_expr
  | CAbs of identifier list * cc_expr
  | CIfThenElse of cc_expr * cc_expr * cc_expr
  | CLetIn of cc_binding * cc_expr

and cc_binding =
  | CVal of identifier * cc_expr
  | CFun of identifier * identifier list * cc_expr
[@@deriving show { with_path = false }]

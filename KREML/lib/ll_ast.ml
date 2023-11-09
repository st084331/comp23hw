open Ast

(** Simplified ast representation which is the result of lambda lifting stage.
    The difference with cc_expr is that there are no nested functions (fun in let-in expr) and anonymous functions *)


type ll_expr =
  | LLiteral of literal
  | LIdentifier of identifier
  | LUnaryOp of unary_op * ll_expr
  | LBinaryOp of binary_op * ll_expr * ll_expr
  | LApp of ll_expr * ll_expr
  | LIfThenElse of ll_expr * ll_expr * ll_expr
  | LLetIn of identifier * ll_expr * ll_expr
[@@deriving show { with_path = false }]

type ll_binding =
  | LVal of identifier * ll_expr (* val x = 88 *)
  | LFun of identifier * identifier list * ll_expr (* fun sqr x = x * x *)
[@@deriving show { with_path = false }]

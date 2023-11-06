open Ast

(** Simplified ast representation which is the result of lambda lifting stage.
    The difference with cc_expr is that there are no let_in expr and anonymous functions *)

type ll_expr =
  | LLiteral of literal
  | LIdentifier of identifier
  | LUnaryOp of unary_op * ll_expr
  | LBinaryOp of binary_op * ll_expr * ll_expr
  | LApp of ll_expr * ll_expr
  | LIfThenElse of ll_expr * ll_expr * ll_expr
[@@deriving show { with_path = false }]

type ll_binding =
  | LVal of identifier * ll_expr (* val x = 88 *)
  | LFun of identifier * identifier list * ll_expr (* fun sqr x = x * x *)
[@@deriving show { with_path = false }]

(* smart constructors *)

(* expressions *)
let ll_literal x = LLiteral x
let ll_identifier x = LIdentifier x
let ll_unary_op op x = LUnaryOp (op, x)
let ll_app x1 x2 = LApp (x1, x2)
let ll_if_then_else cond if_true if_false = LIfThenElse (cond, if_true, if_false)
let ll_binary_op op left right = LBinaryOp (op, left, right)

(* bindings *)
let ll_val value_id expression = LVal (value_id, expression)
let ll_fun fun_id args body = LFun (fun_id, args, body)

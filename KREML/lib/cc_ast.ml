open Ast

(** Simplified ast representation which is the result of closure conversion stage.
    The only difference is that there's 1 binding in let_in expr, not a list*)

type cc_expr =
  | CLiteral of literal
  | CIdentifier of identifier
  | CUnaryOp of unary_op * cc_expr
  | CBinaryOp of binary_op * cc_expr * cc_expr
  | CApp of cc_expr * cc_expr
  | CAbs of identifier * cc_expr
  | CIfThenElse of cc_expr * cc_expr * cc_expr
  | CLetIn of cc_binding * cc_expr

and cc_binding =
  | CVal of identifier * cc_expr (* val x = 88 *)
  | CFun of identifier * identifier list * cc_expr (* fun sqr x = x * x *)
[@@deriving show { with_path = false }]

(* smart constructors *)

(* expressions *)
let cc_literal x = CLiteral x
let cc_identifier x = CIdentifier x
let cc_unary_op op x = CUnaryOp (op, x)
let cc_app x1 x2 = CApp (x1, x2)
let cc_abs arg body = CAbs (arg, body)
let cc_if_then_else cond if_true if_false = CIfThenElse (cond, if_true, if_false)
let cc_binary_op op left right = CBinaryOp (op, left, right)
let cc_let_in binding body = CLetIn (binding, body)

(* bindings *)
let cc_val value_id expression = CVal (value_id, expression)
let cc_fun fun_id args body = CFun (fun_id, args, body)

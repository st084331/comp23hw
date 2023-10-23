val is_space : char -> bool
val skip : string Angstrom.t
val trim : 'a Angstrom.t -> 'a Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val varname : string Angstrom.t
type dispatch = {
  literal_p : Ast.expr Angstrom.t;
  identifier_p : Ast.expr Angstrom.t;
  unary_op_p : dispatch -> Ast.expr Angstrom.t;
  binary_op_p : dispatch -> Ast.expr Angstrom.t;
  app_p : dispatch -> Ast.expr Angstrom.t;
  abs_p : dispatch -> Ast.expr Angstrom.t;
  if_then_else_p : dispatch -> Ast.expr Angstrom.t;
  let_in_p : dispatch -> Ast.expr Angstrom.t;
}
val binding_p : dispatch -> Ast.binding Angstrom.t
val literal_p : Ast.expr Angstrom.t
val identifier_p : Ast.expr Angstrom.t
val unary_op_p : dispatch -> Ast.expr Angstrom.t
val binary_op_p : dispatch -> Ast.expr Angstrom.t
val app_p : dispatch -> Ast.expr Angstrom.t
val abs_p : dispatch -> Ast.expr Angstrom.t
val if_then_else_p : dispatch -> Ast.expr Angstrom.t
val let_in_p : dispatch -> Ast.expr Angstrom.t
val dispatch : dispatch
val parse : string -> (Ast.binding list, string) result
val parse_optimistically : string -> Ast.binding list
val parse_error : string -> bool

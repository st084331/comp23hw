val parse : string -> (Ast.binding list, string) result
val parse_optimistically : string -> Ast.binding list
val parse_error : string -> bool

open Ast
open Angstrom

(* helpers *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip = take_while is_space
let trim p = skip *> p <* skip
let parens p = skip *> char '(' *> skip *> p <* skip <* char ')'

let varname =
  let keywords =
    [ "fun"
    ; "val"
    ; "if"
    ; "then"
    ; "else"
    ; "in"
    ; "fn"
    ; "true"
    ; "false"
    ; "not"
    ; "orelse"
    ; "andalso"
    ]
  in
  let is_valid_first_char = function
    | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_varname_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  skip *> peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_varname_char
    >>= fun s ->
    if List.exists (fun keyword -> keyword = s) keywords
    then fail "Invalid variable name: keyword used"
    else return s
  | _ -> fail "Invalid variable name: bad first char"
;;

(* dispatch *)
type dispatch =
  { literal_p : expr t
  ; identifier_p : expr t
  ; unary_op_p : dispatch -> expr t
  ; binary_op_p : dispatch -> expr t
  ; app_p : dispatch -> expr t
  ; abs_p : dispatch -> expr t
  ; if_then_else_p : dispatch -> expr t
  ; let_in_p : dispatch -> expr t
  }

(* declaration parser *)
let declaration_p d =
  fix
  @@ fun self ->
  let name keyword =
    string keyword *> skip *> d.identifier_p
    >>= function
    | EIdentifier id -> return id
    | _ -> fail "Invalid binding name"
  in
  let args = skip *> many (varname <* skip) in
  let body =
    skip
    *> char '='
    *> choice
         [ d.literal_p
         ; d.identifier_p
         ; d.unary_op_p d
         ; d.binary_op_p d
         ; d.app_p d
         ; d.abs_p d
         ; d.if_then_else_p d
         ; d.let_in_p d
         ]
  in
  skip
  *> (parens self <|> lift2 d_val (name "val") body <|> lift3 d_fun (name "fun") args body)
;;

(* expression parsers *)
let literal_p =
  fix
  @@ fun self ->
  let boolean = string "true" <|> string "false" >>| bool_of_string in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let int_literal = integer >>| fun x -> LInt x in
  let bool_literal = boolean >>| fun x -> LBool x in
  let literal = choice [ int_literal; bool_literal ] in
  skip *> (parens self <|> lift e_literal literal)
;;

let identifier_p =
  fix
  @@ fun self ->
  let is_wildcard c = c == "_" in
  let id =
    varname
    >>= function
    | id when not (is_wildcard id) -> return @@ e_identifier id
    | _ -> fail "Invalid variable name"
  in
  skip *> (parens self <|> id)
;;

let unary_op_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let op = choice [ char '~' >>| uneg; string "not" >>| unot ] in
  let body =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; self
      ; d.binary_op_p d
      ; d.app_p d
      ; d.abs_p d
      ; d.if_then_else_p d
      ; d.let_in_p d
      ]
  in
  skip *> lift2 e_unary_op op body
;;

let binary_op_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let multiplicative = skip *> choice [ char '*' >>| bmul; char '/' >>| bdiv ] in
  let additive = skip *> choice [ char '+' >>| badd; char '-' >>| bsub ] in
  let relational =
    skip
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blt
         ]
  in
  let equality = skip *> string "=" >>| beq in
  let logical_and = skip *> string "andalso" >>| band in
  let logical_or = skip *> string "orelse" >>| bor in
  let rec parse_bin_op expr_parser op_parsers =
    let chainl1 expr_p op_p =
      let rec go acc =
        lift2 (fun f x -> e_binary_op f acc x) op_p expr_p >>= go <|> return acc
      in
      expr_p >>= fun init -> go init
    in
    match op_parsers with
    | [ op ] -> chainl1 expr_parser op
    | h :: t -> chainl1 (parse_bin_op expr_parser t) h
    | _ -> fail "Unreachable"
  in
  let body =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; d.unary_op_p d
      ; self
      ; d.app_p d
      ; d.abs_p d
      ; d.if_then_else_p d
      ; d.let_in_p d
      ]
  in
  skip
  *> parse_bin_op
       body
       [ logical_or; logical_and; equality; relational; additive; multiplicative ]
;;

let app_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let expr =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; self
      ; d.abs_p d
      ; d.if_then_else_p d
      ; d.let_in_p d
      ]
  in
  skip *> lift2 e_app expr expr
;;

let abs_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let arg = skip *> string "fn" *> skip *> varname <* skip <* string "=>" in
  let body =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.app_p d
      ; self
      ; d.if_then_else_p d
      ; d.let_in_p d
      ]
  in
  skip *> lift2 e_abs arg body
;;

let if_then_else_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let expr =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.app_p d
      ; d.abs_p d
      ; self
      ; d.let_in_p d
      ]
  in
  let cond = skip *> string "if" *> expr in
  let if_true = skip *> string "then" *> expr in
  let if_false = skip *> string "else" *> expr in
  skip *> lift3 e_if_then_else cond if_true if_false
;;

let let_in_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let expr =
    choice
      [ d.literal_p
      ; d.identifier_p
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.app_p d
      ; d.abs_p d
      ; d.if_then_else_p d
      ; self
      ]
  in
  let declarations = string "let" *> skip *> sep_by1 skip (declaration_p d) in
  let body = skip *> string "in" *> expr <* skip <* string "end" in
  skip *> lift2 e_let_in declarations body
;;

(* main parser *)
let dispatch =
  { literal_p
  ; identifier_p
  ; unary_op_p
  ; binary_op_p
  ; app_p
  ; abs_p
  ; if_then_else_p
  ; let_in_p
  }
;;

let parse program =
  parse_string ~consume:All (many (declaration_p dispatch) <* skip) program
;;

let parse_optimistically program = Result.get_ok (parse program)

(* tests *)
let%test _ =
  parse_optimistically "val x = (5 + 5)"
  = [ DVal ("x", EBinaryOp (Add, ELiteral (LInt 5), ELiteral (LInt 5))) ]
;;

open Ast
open Angstrom

(* helpers *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip = take_while is_space
let spaces = take_while1 is_space
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
  { expression_p : dispatch -> expr t
  ; declaration_p : dispatch -> decl t
  }

let literal_p =
  fix
  @@ fun self ->
  skip
  *>
  let boolean = string "true" <|> string "false" >>| bool_of_string in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let int_literal_p = integer >>| fun x -> LInt x in
  let bool_literal_p = boolean >>| fun x -> LBool x in
  let literal = choice [ int_literal_p; bool_literal_p ] in
  parens self <|> lift e_literal literal
;;

let identifier_p =
  fix
  @@ fun self ->
  skip
  *>
  let is_wildcard c = c == "_" in
  let id =
    varname
    >>= function
    | id when not (is_wildcard id) -> return @@ e_identifier id
    | _ -> fail "Invalid variable name"
  in
  parens self <|> id
;;

let unary_op_p d =
  fix
  @@ fun self ->
  skip
  *>
  let op = choice [ char '~' >>| uneg; string "not" >>| unot ] in
  parens self <|> lift2 e_unary_op op (d.expression_p d)
;;

let binary_op_p d =
  fix
  @@ fun self ->
  skip
  *>
  let multiplicative = spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ] in
  let additive = spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ] in
  let relational =
    spaces
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blt
         ]
  in
  let equality = spaces *> string "=" >>| beq in
  let logical_and = spaces *> string "andalso" >>| band in
  let logical_or = spaces *> string "orelse" >>| bor in
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
  parens self
  <|> parse_bin_op
        (d.expression_p d)
        [ logical_or; logical_and; equality; relational; additive; multiplicative ]
;;

let app_p d =
  fix
  @@ fun self ->
  skip
  *>
  let expr = spaces *> d.expression_p d <* spaces in
  parens self <|> lift2 e_app expr expr
;;

let abs_p d =
  fix
  @@ fun self ->
  skip
  *>
  let keyword = skip *> string "fn" *> spaces in
  let arg = varname in
  let body = spaces *> string "=>" *> d.expression_p d in
  parens self <|> keyword *> lift2 e_abs arg body
;;

let if_then_else_p d =
  fix
  @@ fun self ->
  skip
  *>
  let cond = spaces *> string "if" *> d.expression_p d in
  let if_true = spaces *> string "then" *> d.expression_p d in
  let if_false = spaces *> string "else" *> d.expression_p d in
  parens self <|> lift3 e_if_then_else cond if_true if_false
;;

let let_in_p d =
  fix
  @@ fun self ->
  skip
  *>
  let keyword = string "let" *> skip in
  let declarations = sep_by1 spaces (d.declaration_p d) in
  let body = skip *> string "in" *> d.expression_p d <* skip <* string "end" in
  parens self <|> keyword *> lift2 e_let_in declarations body
;;

let expression_p d =
  choice
    [ literal_p
    ; identifier_p
    ; unary_op_p d
    ; binary_op_p d
    ; app_p d
    ; abs_p d
    ; if_then_else_p d
    ; let_in_p d
    ]
;;

(* declaration parser *)
let declaration_p d =
  fix
  @@ fun self ->
  skip
  *>
  let name keyword =
    string keyword *> spaces *> identifier_p
    >>= function
    | EIdentifier id -> return id
    | _ -> fail "Invalid binding name"
  in
  let args = skip *> many (varname <* skip) in
  let body = trim (char '=') *> d.expression_p d in
  parens self <|> lift2 d_val (name "val") body <|> lift3 d_fun (name "fun") args body
;;

(* main parser *)
let dispatch = { expression_p; declaration_p }

let parse program =
  parse_string ~consume:All (many (declaration_p dispatch) <* skip) program
;;

let parse_optimistically program = Result.get_ok (parse program)

(* tests *)
let%test _ = parse_optimistically "val x = x" = [ DVal ("x", EIdentifier "x") ]

(* let x program = parse_string ~consume:All varname program
let _ =
  x "x"
  |> function
  | Ok a | Error a -> print_string a
;; *)

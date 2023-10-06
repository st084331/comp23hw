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
  peek_char
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

(* expression parsers *)
let literal_p = fail "TODO"

let identifier_p =
  let is_wildcard c = c == "_" in
  varname
  >>= function
  | id when not (is_wildcard id) -> return @@ e_identifier id
  | _ -> fail "Invalid variable name"
;;

let unary_op_p d = fail "TODO"
let binary_op_p d = fail "TODO"
let app_p d = fail "TODO"
let abs_p d = fail "TODO"
let if_then_else_p d = fail "TODO"

let let_in_p d =
  let keyword = skip *> string "let" *> spaces in
  let declarations = sep_by1 (trim (char ',')) (d.declaration_p d) in
  let body = spaces *> string "in" *> d.expression_p d <* spaces <* string "end" in
  keyword *> lift2 e_let_in declarations body
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

(* declaration parsers *)
let val_p d =
  let name = trim (string "val") *> varname in
  let body = trim (char '=') *> d.expression_p d in
  lift2 d_val name body
;;

let fun_p d =
  let name =
    skip *> string "fun" *> spaces *> identifier_p
    >>= function
    | EIdentifier id -> return id
    | _ -> fail "Invalid function name"
  in
  let args = skip *> many (varname <* skip) in
  let body = trim (char '=') *> d.expression_p d in
  lift3 d_fun name args body
;;

let declaration_p d = val_p d <|> fun_p d

(* main parser *)
let default_dispatch = { expression_p; declaration_p }

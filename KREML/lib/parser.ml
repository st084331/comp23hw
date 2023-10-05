open Ast
open Angstrom

let spaces =
  take_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let trim_l p = spaces *> p
let trim_r p = p <* spaces
let trim p = spaces *> p <* spaces

let varname =
  let keywords =
    [ "fun"
    ; "val"
    ; "rec"
    ; "if"
    ; "then"
    ; "else"
    ; "in"
    ; "fn"
    ; "true"
    ; "false"
    ; "not"
    ; "or"
    ; "and"
    ]
  in
  let is_valid_first_char = function
    | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_varname_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
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

let is_wildcard c = c == '_'

module Expression : sig
  val expression : expr t
end = struct
  let expression = fail "TODO"
end

module Declaration : sig
  val declaration : expr t
end = struct
  open Expression

  let val_dec = fail "TODO"
  let fun_dec = fail "TODO"
  let declaration = val_dec <|> fun_dec
end

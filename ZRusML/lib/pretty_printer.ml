open Ast
open Format

let pp_const fmt = function
  | CInt x -> fprintf fmt "%d" x
  | CBool x -> fprintf fmt "%b" x
;;

let pp_bin_op fmt =
  let printer = fprintf fmt in
  function
  | And -> printer "+"
  | Or -> printer "or"
  | Less -> printer "<"
  | Leq -> printer "<="
  | Gre -> printer ">"
  | Geq -> printer ">="
  | Eq -> printer "="
  | Neq -> printer "<>"
  | Add -> printer "+"
  | Sub -> printer "-"
  | Mul -> printer "*"
  | Div -> printer "/"
;;

let pp_un_op fmt = function
  | Not -> fprintf fmt "not"
  | Minus -> fprintf fmt "-"
;;

let pp_pt fmt = function
  | PtWild -> fprintf fmt "_"
  | PtVar x -> fprintf fmt "%s" x
  | PtConst x -> pp_const fmt x
;;

let print_tabs fmt cnt =
  fprintf fmt "%s" (String.concat "" (List.init cnt (fun _ -> "\t")))
;;

let rec pp_exp fmt cnt = function
  | EConst x -> pp_const fmt x
  | EUnOp (o, e) ->
    pp_un_op fmt o;
    pp_exp fmt cnt e
  | EVar id -> fprintf fmt "%s" id
  | EIf (predicate, true_branch, false_branch) ->
    fprintf fmt "if ";
    pp_exp fmt cnt predicate;
    fprintf fmt " then ";
    pp_exp fmt cnt true_branch;
    fprintf fmt " else ";
    pp_exp fmt cnt false_branch
  | EBinOp (op, e1, e2) ->
    let start, stop =
      match op with
      | Sub | Add -> "(", ")"
      | _ -> "", ""
    in
    fprintf fmt "%s" start;
    pp_exp fmt cnt e1;
    fprintf fmt " ";
    pp_bin_op fmt op;
    fprintf fmt " ";
    pp_exp fmt cnt e2;
    fprintf fmt "%s" stop
  | EFun (_, _) as orig ->
    let rec helper = function
      | EFun (a, b) ->
        pp_pt fmt a;
        fprintf fmt " ";
        helper b
      | exp ->
        fprintf fmt "-> ";
        pp_exp fmt cnt exp
    in
    fprintf fmt "(fun ";
    helper orig;
    fprintf fmt ")"
  | ELet (bindings, e) ->
    let cnt = cnt + 1 in
    fprintf fmt "(\n";
    List.iter
      (fun (is_rec, pt, exp) ->
        print_tabs fmt cnt;
        fprintf fmt "let ";
        fprintf fmt (if is_rec then "rec " else "");
        pp_pt fmt pt;
        fprintf fmt " = ";
        pp_exp fmt cnt exp;
        fprintf fmt " in\n")
      bindings;
    print_tabs fmt cnt;
    pp_exp fmt cnt e;
    fprintf fmt "\n";
    print_tabs fmt (cnt - 1);
    fprintf fmt ") "
  | EApp (e1, e2) ->
    pp_exp fmt cnt e1;
    fprintf fmt " ";
    pp_exp fmt cnt e2
;;

let pp_binding fmt cnt (is_rec, pt, exp) =
  print_tabs fmt cnt;
  fprintf fmt "let ";
  fprintf fmt (if is_rec then "rec " else "");
  pp_pt fmt pt;
  fprintf fmt " = ";
  pp_exp fmt cnt exp
;;

let pp_decl fmt (DLet (is_rec, pt, exp)) =
  pp_binding fmt 0 (is_rec, pt, exp);
  fprintf fmt ";;\n"
;;

let pp_prog fmt prog = List.iter (fun elem -> pp_decl fmt elem) prog

open Anf
open Format
open Pretty_printer

let show_immexpr = function
  | ImmInt x -> sprintf "%d" x
  | ImmBool x -> sprintf "%b" x
  | ImmIdentifier x -> x
;;

let pp_immexpr fmt ie = fprintf fmt "%s" (show_immexpr ie)

let show_cexpr = function
  | CImmExpr x -> show_immexpr x
  | CUnaryOp (o, x) -> sprintf "%s%s" (show_un_op o) (show_immexpr x)
  | CBinaryOp (o, x1, x2) ->
    sprintf "%s %s %s" (show_immexpr x1) (show_bin_op o) (show_immexpr x2)
  | CApp (f, x) -> sprintf "%s %s" (show_immexpr f) (show_immexpr x)
  | CIf (condition, true_branch, false_branch) ->
    sprintf
      "if %s then %s else %s"
      (show_immexpr condition)
      (show_immexpr true_branch)
      (show_immexpr false_branch)
;;

let pp_cexpr fmt ce = fprintf fmt "%s" (show_cexpr ce)

let show_pexpr = function
  | PImmExpr x -> show_immexpr x
  | PImmWild -> "_"
;;

let pp_pexpr fmt pe = fprintf fmt "%s" pe

let rec show_aexpr = function
  | ALet (id, ce, ae) ->
    sprintf "    let %s = %s in\n%s" id (show_cexpr ce) (show_aexpr ae)
  | ACExpr ce -> show_cexpr ce
;;

let pp_aexpr fmt ae = fprintf fmt "%s" (show_aexpr ae)

let show_abinding (ABind (is_rec, id, lst, ae)) =
  let rec_str = if is_rec then "rec " else "" in
  let rec construct_lst = function
    | hd :: tl -> sprintf "%s %s" (show_pexpr hd) (construct_lst tl)
    | _ -> ""
  in
  let jump =
    match ae with
    | ALet _ -> "\n"
    | _ -> " "
  in
  let lst_str = construct_lst lst in
  sprintf "let %s%s %s=%s%s;;\n\n" rec_str id lst_str jump (show_aexpr ae)
;;

let pp_abinding fmt ab = fprintf fmt "%s" (show_abinding ab)

let rec show_abinding_list = function
  | hd :: tl -> sprintf "%s%s" (show_abinding hd) (show_abinding_list tl)
  | _ -> ""
;;

let pp_abinding_list fmt lst = fprintf fmt "%s" (show_abinding_list lst)

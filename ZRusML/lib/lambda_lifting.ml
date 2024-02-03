(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
module StringMap = Map.Make (String)

(* THIS FUNCTIONS CREATES DUMMY ID THAT CANNOT BE USED IN CODE AND MUST BE RENAMED FURTHER *)
let get_lifted_name cnt = string_of_int cnt ^ "lambda"
let z_combinator_name = "/Z"

let z_combinator =
  DLet
    ( true
    , PtVar z_combinator_name
    , EFun
        ( PtVar "f"
        , EFun
            ( PtVar "x"
            , EApp (EApp (EVar "f", EApp (EVar z_combinator_name, EVar "f")), EVar "x") )
        ) )
;;

let lift_rec lst cnt pt exp =
  let new_lambda = get_lifted_name cnt in
  let lst = DLet (false, PtVar new_lambda, EFun (pt, exp)) :: lst in
  lst, cnt + 1, EApp (EVar z_combinator_name, EVar new_lambda)
;;

let rec lift_exp lst cnt exp =
  match exp with
  | EUnOp (op, e) ->
    let new_lst, new_cnt, new_e = lift_exp lst cnt e in
    new_lst, new_cnt, EUnOp (op, new_e)
  | ELet (bindings, e) ->
    let (lst, cnt), new_bindings =
      List.fold_left_map
        (fun (acc_lst, acc_cnt) decl ->
          let next_lst, next_cnt, new_decl = lift_decl acc_lst acc_cnt decl in
          (next_lst, next_cnt), new_decl)
        (lst, cnt)
        bindings
    in
    let final_lst, final_cnt, new_e = lift_exp lst cnt e in
    final_lst, final_cnt, ELet (new_bindings, new_e)
  | EFun _ ->
    let e =
      let rec helper_efun = function
        | EFun (_, ex) -> helper_efun ex
        | other -> other
      in
      helper_efun exp
    in
    let lst, cnt, new_e = lift_exp lst cnt e in
    let name = get_lifted_name cnt in
    let new_efun =
      let rec helper_result = function
        | EFun (p, e) -> EFun (p, helper_result e)
        | _ -> new_e
      in
      helper_result exp
    in
    let lst = DLet (false, PtVar name, new_efun) :: lst in
    lst, cnt + 1, EVar name
  | EIf (e1, e2, e3) ->
    let lst, cnt, exp1 = lift_exp lst cnt e1 in
    let lst, cnt, exp2 = lift_exp lst cnt e2 in
    let lst, cnt, exp3 = lift_exp lst cnt e3 in
    lst, cnt, EIf (exp1, exp2, exp3)
  | EBinOp (op, e1, e2) ->
    let map, cnt, exp1 = lift_exp lst cnt e1 in
    let map, cnt, exp2 = lift_exp map cnt e2 in
    map, cnt, EBinOp (op, exp1, exp2)
  | EApp (e1, e2) ->
    let map, cnt, exp1 = lift_exp lst cnt e1 in
    let map, cnt, exp2 = lift_exp map cnt e2 in
    map, cnt, EApp (exp1, exp2)
  | _ -> lst, cnt, exp

and lift_decl lst cnt (is_rec, pt, exp) =
  let lst, cnt, exp = lift_exp lst cnt exp in
  let lst, cnt, exp = if is_rec then lift_rec lst cnt pt exp else lst, cnt, exp in
  lst, cnt, (false, pt, exp)
;;

let lift_prog prog =
  let new_bindings, _ =
    List.fold_left
      (fun (acc_lst, acc_cnt) (DLet decl) ->
        let next_lst, next_cnt, new_decl = lift_decl [] acc_cnt decl in
        let next_lst = DLet new_decl :: next_lst in
        acc_lst @ List.rev next_lst, next_cnt)
      ([ z_combinator ], 0)
      prog
  in
  new_bindings
;;

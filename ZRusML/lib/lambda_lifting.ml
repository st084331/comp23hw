(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Parser
open Pretty_printer
module StringMap = Map.Make (String)

let rec lift_exp map cnt exp =
  match exp with
  | EUnOp (op, e) ->
    let new_map, new_cnt, new_e = lift_exp map cnt e in
    new_map, new_cnt, EUnOp (op, new_e)
  | ELet (bindings, e) ->
    let (map, cnt), new_bindings =
      List.fold_left_map
        (fun (acc_map, acc_cnt) (is_rec, pt, elem) ->
          let next_map, next_cnt, new_elem = lift_exp acc_map acc_cnt elem in
          (next_map, next_cnt), (is_rec, pt, new_elem))
        (map, cnt)
        bindings
    in
    let final_map, final_cnt, new_e = lift_exp map cnt e in
    final_map, final_cnt, ELet (new_bindings, new_e)
  | EFun _ ->
    let rec helper_efun = function
      | EFun (_, ex) -> helper_efun ex
      | other -> other
    in
    let e = helper_efun exp in
    let map, cnt, new_e = lift_exp map cnt e in
    let name = string_of_int cnt ^ "lambda" in
    let rec helper_result = function
      | EFun (p, e) -> EFun (p, helper_result e)
      | _ -> new_e
    in
    let new_efun = helper_result exp in
    let new_map = StringMap.add name new_efun map in
    new_map, cnt + 1, EVar name
  | EIf (e1, e2, e3) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    let map3, cnt3, exp3 = lift_exp map2 cnt2 e3 in
    map3, cnt3, EIf (exp1, exp2, exp3)
  | EBinOp (op, e1, e2) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    map2, cnt2, EBinOp (op, exp1, exp2)
  | EApp (e1, e2) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    map2, cnt2, EApp (exp1, exp2)
  | _ -> map, cnt, exp
;;

let lift_decl map cnt (DLet (is_rec, pt, exp)) =
  let new_map, new_cnt, new_exp = lift_exp map cnt exp in
  new_map, new_cnt, DLet (is_rec, pt, new_exp)
;;

let lift_prog prog =
  let (new_bindings, _), new_prog =
    List.fold_left_map
      (fun (acc_map, acc_cnt) (DLet (is_rec, pt, elem)) ->
        let next_map, next_cnt, new_elem = lift_exp acc_map acc_cnt elem in
        (next_map, next_cnt), DLet (is_rec, pt, new_elem))
      (StringMap.empty, 0)
      prog
  in
  let new_bindings =
    List.map (fun (name, e) -> DLet (true, PtVar name, e))
    @@ List.of_seq
    @@ StringMap.to_seq new_bindings
  in
  new_bindings @ new_prog
;;

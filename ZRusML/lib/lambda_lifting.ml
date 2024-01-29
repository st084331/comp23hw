(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Pretty_printer
module StringMap = Map.Make (String)

(* THIS FUNCTION CREATES DUMMY ID THAT CANNOT BE USED IN CODE AND MUST BE RENAMED FURTHER *)
let get_lifted_name cnt = "lambda" ^ string_of_int cnt
let z_combinator_name = "ZOV"

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

let rec replace_exp old_exp new_exp = function
  | x when x = old_exp -> new_exp
  | EUnOp (op, e) -> EUnOp (op, replace_exp old_exp new_exp e)
  | ELet (bindings, e) ->
    let new_bindings =
      List.map (fun (r, p, exp) -> r, p, replace_exp old_exp new_exp exp) bindings
    in
    ELet (new_bindings, replace_exp old_exp new_exp e)
  | EFun (p, e) -> EFun (p, replace_exp old_exp new_exp e)
  | EIf (e1, e2, e3) ->
    EIf
      ( replace_exp old_exp new_exp e1
      , replace_exp old_exp new_exp e2
      , replace_exp old_exp new_exp e3 )
  | EBinOp (op, e1, e2) ->
    EBinOp (op, replace_exp old_exp new_exp e1, replace_exp old_exp new_exp e2)
  | EApp (e1, e2) -> EApp (replace_exp old_exp new_exp e1, replace_exp old_exp new_exp e2)
  | _ as original -> original
;;

let rec lift_exp map cnt exp =
  match exp with
  | EUnOp (op, e) ->
    let new_map, new_cnt, new_e = lift_exp map cnt e in
    new_map, new_cnt, EUnOp (op, new_e)
  | ELet (bindings, e) ->
    let (map, cnt), new_bindings =
      List.fold_left_map
        (fun (acc_map, acc_cnt) decl ->
          let next_map, next_cnt, new_decl = lift_decl acc_map acc_cnt decl in
          let next_map, next_cnt, new_decl =
            match new_decl with
            | _, pt, EFun (e1, e2) ->
              let next_map, next_cnt, new_exp =
                lift_exp next_map next_cnt (EFun (e1, e2))
              in
              let next_map =
                match pt, new_exp with
                | PtVar id, EVar lambda ->
                  let (DLet (_, pt, e)) = StringMap.find lambda next_map in
                  let new_rec, _, _ = decl in
                  StringMap.add
                    lambda
                    (DLet (new_rec, pt, replace_exp (EVar id) (EVar lambda) e))
                    next_map
                | _ -> next_map
              in
              next_map, next_cnt, (false, pt, new_exp)
            | other -> next_map, next_cnt, other
          in
          (next_map, next_cnt), new_decl)
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
    let name = get_lifted_name cnt in
    let rec helper_result = function
      | EFun (p, e) -> EFun (p, helper_result e)
      | _ -> new_e
    in
    let new_efun = helper_result exp in
    let new_map = StringMap.add name (DLet (false, PtVar name, new_efun)) map in
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

and lift_decl map cnt (is_rec, pt, exp) =
  let map, cnt, exp = lift_exp map cnt exp in
  let rec decompose_eapp args = function
    | EApp (e, arg) -> decompose_eapp (arg :: args) e
    | other -> other, args
  in
  let fn, args = decompose_eapp [] exp in
  let fn, map =
    match fn with
    | EVar id ->
      (match StringMap.find_opt id map with
       | Some (DLet (_, _, e)) -> e, StringMap.remove id map
       | _ -> fn, map)
    | _ -> fn, map
  in
  let rec construct_exp = function
    | EFun (PtVar id, e), h :: tl ->
      let e' = replace_exp (EVar id) h e in
      construct_exp (e', tl)
    | EFun (_, e), _ :: tl -> construct_exp (e, tl)
    | other, _ -> other
  in
  map, cnt, (is_rec, pt, construct_exp (fn, args))
;;

let lift_prog prog =
  let (new_bindings, _), new_prog =
    List.fold_left_map
      (fun (acc_map, acc_cnt) (DLet decl) ->
        let next_map, next_cnt, new_decl = lift_decl acc_map acc_cnt decl in
        (next_map, next_cnt), DLet new_decl)
      (StringMap.add z_combinator_name z_combinator StringMap.empty, 0)
      prog
  in
  let new_bindings = List.map snd @@ List.of_seq @@ StringMap.to_seq new_bindings in
  new_bindings @ new_prog
;;

(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

let counter = ref 0

let fresh_var () =
  let var = "v" ^ string_of_int !counter in
  counter := !counter + 1;
  var
;;

(* Transform an expression into ANF *)
let rec exp_to_anf exp =
  match exp with
  | EConst _ | EVar _ -> exp, []
  | EUnOp (op, e) ->
    let e_anf, e_lets = exp_to_anf e in
    (match e_anf with
     | EVar _ | EConst _ -> EUnOp (op, e_anf), e_lets
     | _ ->
       let var = fresh_var () in
       EVar var, e_lets @ [ false, PtVar var, EUnOp (op, e_anf) ])
  | EBinOp (op, e1, e2) ->
    let e1_anf, e1_lets = exp_to_anf e1 in
    let e2_anf, e2_lets = exp_to_anf e2 in
    let var = fresh_var () in
    EVar var, e1_lets @ e2_lets @ [ false, PtVar var, EBinOp (op, e1_anf, e2_anf) ]
  | EIf (cond, e_then, e_else) ->
    let cond_anf, cond_lets = exp_to_anf cond in
    let then_anf, then_lets = exp_to_anf e_then in
    let else_anf, else_lets = exp_to_anf e_else in
    let var = fresh_var () in
    ( EVar var
    , cond_lets
      @ then_lets
      @ else_lets
      @ [ false, PtVar var, EIf (cond_anf, then_anf, else_anf) ] )
  | ELet (bindings, e) ->
    let e_anf, e_lets = exp_to_anf e in
    let bindings_anf, bindings_lets = bindings_to_anf bindings in
    e_anf, bindings_lets @ e_lets
  | EFun (pt, e) ->
    let e_anf, e_lets = exp_to_anf e in
    let var = fresh_var () in
    EVar var, e_lets @ [ false, pt, EFun (pt, e_anf) ]
  | EApp (e1, e2) ->
    let e1_anf, e1_lets = exp_to_anf e1 in
    let e2_anf, e2_lets = exp_to_anf e2 in
    let var = fresh_var () in
    EVar var, e1_lets @ e2_lets @ [ false, PtVar var, EApp (e1_anf, e2_anf) ]
  | _ -> failwith "Not implemented"

(* Transform a list of bindings into ANF *)
and bindings_to_anf bindings =
  List.fold_right
    (fun (is_rec, pat, e) (acc_exp, acc_lets) ->
      let e_anf, e_lets = exp_to_anf e in
      let var = fresh_var () in
      EVar var, e_lets @ acc_lets @ [ is_rec, pat, EVar var ])
    bindings
    (EVar (fresh_var ()), [])
;;

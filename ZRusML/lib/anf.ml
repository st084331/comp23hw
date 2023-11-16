(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type error =
  | NotImplemented
  | OtherError of string

let counter = ref 0
let reset_counter () = counter := 0

let fresh_var () =
  let var = "v" ^ string_of_int !counter in
  counter := !counter + 1;
  var
;;

(* Transform an expression into ANF *)
let rec exp_to_anf exp : (exp * binding list, error) result =
  match exp with
  | EConst _ | EVar _ -> Ok (exp, [])
  | EUnOp (op, e) ->
    (match exp_to_anf e with
     | Ok (e_anf, e_lets) ->
       let var = fresh_var () in
       Ok (EVar var, e_lets @ [ false, PtVar var, EUnOp (op, e_anf) ])
     | Error _ as e -> e)
  | EBinOp (op, e1, e2) ->
    (match exp_to_anf e1, exp_to_anf e2 with
     | Ok (e1_anf, e1_lets), Ok (e2_anf, e2_lets) ->
       let var = fresh_var () in
       Ok (EVar var, e1_lets @ e2_lets @ [ false, PtVar var, EBinOp (op, e1_anf, e2_anf) ])
     | (Error _ as e), _ | _, (Error _ as e) -> e)
  | EIf (cond, e_then, e_else) ->
    (match exp_to_anf cond, exp_to_anf e_then, exp_to_anf e_else with
     | Ok (cond_anf, cond_lets), Ok (then_anf, then_lets), Ok (else_anf, else_lets) ->
       let var = fresh_var () in
       Ok
         ( EVar var
         , cond_lets
           @ then_lets
           @ else_lets
           @ [ false, PtVar var, EIf (cond_anf, then_anf, else_anf) ] )
     | (Error _ as e), _, _ | _, (Error _ as e), _ | _, _, (Error _ as e) -> e)
  | ELet (bindings, e) ->
    (match bindings_to_anf bindings, exp_to_anf e with
     | Ok (bindings_anf, bindings_lets), Ok (e_anf, e_lets) ->
       Ok (e_anf, bindings_lets @ e_lets)
     | (Error _ as e), _ | _, (Error _ as e) -> e)
  | EFun (pt, e) ->
    (match exp_to_anf e with
     | Ok (e_anf, e_lets) ->
       let var = fresh_var () in
       Ok (EVar var, e_lets @ [ false, pt, EFun (pt, e_anf) ])
     | Error _ as e -> e)
  | EApp (e1, e2) ->
    (match exp_to_anf e1, exp_to_anf e2 with
     | Ok (e1_anf, e1_lets), Ok (e2_anf, e2_lets) ->
       let var = fresh_var () in
       Ok (EVar var, e1_lets @ e2_lets @ [ false, PtVar var, EApp (e1_anf, e2_anf) ])
     | (Error _ as e), _ | _, (Error _ as e) -> e)
  | _ -> Error NotImplemented

(* Transform a list of bindings into ANF *)
and bindings_to_anf bindings : (exp * binding list, error) result =
  List.fold_right
    (fun (is_rec, pat, e) acc ->
      match acc, exp_to_anf e with
      | Ok (acc_exp, acc_lets), Ok (e_anf, e_lets) ->
        let var = fresh_var () in
        Ok (EVar var, e_lets @ acc_lets @ [ is_rec, pat, EVar var ])
      | (Error _ as e), _ | _, (Error _ as e) -> e)
    bindings
    (Ok (EVar (fresh_var ()), []))
;;

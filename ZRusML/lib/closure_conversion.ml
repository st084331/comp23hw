(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

let rec free_vars exp bound_vars =
  match exp with
  | EVar id -> if List.mem id bound_vars then [] else [ id ]
  | EConst _ -> []
  | EUnOp (_, e) -> free_vars e bound_vars
  | EBinOp (_, e1, e2) -> List.append (free_vars e1 bound_vars) (free_vars e2 bound_vars)
  | EIf (cond, e1, e2) ->
    List.flatten
      [ free_vars cond bound_vars; free_vars e1 bound_vars; free_vars e2 bound_vars ]
  | ELet (bindings, e) ->
    let new_bound_vars, vars_from_bindings =
      List.fold_left
        (fun (bvars, fvars) (is_rec, pt, exp) ->
          match pt with
          | PtVar id ->
            let new_bvars = if is_rec then id :: bvars else bvars in
            new_bvars, List.append fvars (free_vars exp new_bvars)
          | _ ->
            failwith
              "Pattern matching in let bindings is not supported in this free_vars \
               function")
        (bound_vars, [])
        bindings
    in
    List.append vars_from_bindings (free_vars e new_bound_vars)
  | EFun (pt, e) ->
    (match pt with
     | PtVar id -> free_vars e (id :: bound_vars)
     | _ ->
       failwith
         "Pattern matching in functions is not supported in this free_vars function")
  | EApp (e1, e2) -> List.append (free_vars e1 bound_vars) (free_vars e2 bound_vars)
  | _ -> failwith "free_vars: expression not yet implemented for free variable analysis"
;;

let rec convert_exp exp bound_vars =
  match exp with
  | EConst _ | EVar _ -> exp
  | EUnOp (op, e) -> EUnOp (op, convert_exp e bound_vars)
  | EBinOp (op, e1, e2) ->
    EBinOp (op, convert_exp e1 bound_vars, convert_exp e2 bound_vars)
  | EIf (cond, e_then, e_else) ->
    EIf
      ( convert_exp cond bound_vars
      , convert_exp e_then bound_vars
      , convert_exp e_else bound_vars )
  | ELet (bindings, e) ->
    let bound_vars =
      List.fold_left
        (fun acc (is_rec, pt, _) ->
          match pt with
          | PtVar id -> id :: acc
          | _ -> acc)
        bound_vars
        bindings
    in
    let bindings' =
      List.map (fun (is_rec, pt, exp) -> is_rec, pt, convert_exp exp bound_vars) bindings
    in
    let e' = convert_exp e bound_vars in
    ELet (bindings', e')
  | EFun (PtVar id, body) ->
    let fv = free_vars body [ id ] in
    let closure_body = EFun (PtVar id, convert_exp body (id :: fv)) in
    let closure =
      ELet (List.map (fun var -> false, PtVar var, EVar var) fv, closure_body)
    in
    ELet ([ false, PtVar id, closure ], EVar id)
  | EFun _ ->
    failwith "Pattern matching in functions is not supported in this closure conversion"
  | EApp (e1, e2) ->
    let e1' = convert_exp e1 bound_vars in
    let e2' = convert_exp e2 bound_vars in
    EApp (e1', e2')
  | _ -> failwith "convert_exp: expression not yet implemented for closure conversion"
;;

let closure_convert_program decls =
  let converted_decls =
    List.map
      (function
        | DLet (is_rec, pt, exp) -> DLet (is_rec, pt, convert_exp exp []))
      decls
  in
  converted_decls
;;

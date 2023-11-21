(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Var_counter

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string
[@@deriving show { with_path = false }]

type cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of un_op * immexpr
  | CBinaryOp of bin_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIf of immexpr * immexpr * immexpr
[@@deriving show { with_path = false }]

type aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type abinding = AVal of string * aexpr [@@deriving show { with_path = false }]

let fresh_var = fresh_var "anf"

let rec anf (e : exp) (expr_with_hole : immexpr -> aexpr) : aexpr =
  match e with
  | EConst (CInt n) -> expr_with_hole (ImmInt n)
  | EConst (CBool b) -> expr_with_hole (ImmBool b)
  | EVar x -> expr_with_hole (ImmIdentifier x)
  | EUnOp (op, exp) ->
    anf exp (fun imm ->
      let varname = fresh_var () in
      ALet (varname, CUnaryOp (op, imm), expr_with_hole (ImmIdentifier varname)))
  | EBinOp (op, left, right) ->
    anf left (fun limm ->
      anf right (fun rimm ->
        let varname = fresh_var () in
        ALet (varname, CBinaryOp (op, limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | EApp (e1, e2) ->
    anf e1 (fun e1imm ->
      anf e2 (fun e2imm ->
        let varname = fresh_var () in
        ALet (varname, CApp (e1imm, e2imm), expr_with_hole (ImmIdentifier varname))))
  | EIf (cond, e1, e2) ->
    anf cond (fun condimm ->
      anf e1 (fun e1imm ->
        anf e2 (fun e2imm ->
          let varname = fresh_var () in
          ALet
            (varname, CIf (condimm, e1imm, e2imm), expr_with_hole (ImmIdentifier varname)))))
  | ELet (bindings, body) -> anf_let_bindings bindings body expr_with_hole
  | EFun (pt, body) ->
    let varname = fresh_var () in
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    ALet (varname, CImmExpr (ImmIdentifier "_"), anf_body)

and anf_let_bindings bindings body expr_with_hole =
  match bindings with
  | [] -> anf body expr_with_hole
  | (is_rec, pt, exp) :: rest ->
    anf exp (fun immexpr ->
      let varname = fresh_var () in
      match pt with
      | PtWild -> anf_let_bindings rest body expr_with_hole
      | PtVar id ->
        let new_body = substitute id id (ELet (rest, body)) in
        ALet (id, CImmExpr immexpr, anf_let_bindings rest new_body expr_with_hole)
      | PtConst const -> anf_let_bindings rest body expr_with_hole)

and const_to_immexpr const =
  match const with
  | CInt n -> ImmInt n
  | CBool b -> ImmBool b

and substitute old_id new_id expr =
  match expr with
  | EConst _ -> expr
  | EVar id -> if id = old_id then EVar new_id else expr
  | EUnOp (op, e) -> EUnOp (op, substitute old_id new_id e)
  | EBinOp (op, e1, e2) ->
    EBinOp (op, substitute old_id new_id e1, substitute old_id new_id e2)
  | EApp (e1, e2) -> EApp (substitute old_id new_id e1, substitute old_id new_id e2)
  | EIf (cond, e1, e2) ->
    EIf
      ( substitute old_id new_id cond
      , substitute old_id new_id e1
      , substitute old_id new_id e2 )
  | ELet (bindings, e) ->
    ELet
      (List.map (substitute_in_binding old_id new_id) bindings, substitute old_id new_id e)
  | EFun (pt, e) ->
    (match pt with
     | PtVar id when id = old_id -> EFun (pt, e)
     | _ -> EFun (pt, substitute old_id new_id e))

and substitute_in_binding old_id new_id (is_rec, pt, exp) =
  match pt with
  | PtVar id when id = old_id && not is_rec -> is_rec, pt, exp
  | _ -> is_rec, pt, substitute old_id new_id exp

and anf_fun pt body expr_with_hole =
  let varname = fresh_var () in
  match pt with
  | PtVar arg_id ->
    let anf_body =
      anf (substitute arg_id varname body) (fun imm -> ACExpr (CImmExpr imm))
    in
    ALet (varname, CImmExpr (ImmIdentifier arg_id), anf_body)
  | PtWild ->
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    ALet (varname, CImmExpr (ImmIdentifier "_"), anf_body)
  | PtConst const ->
    let anf_body = anf body (fun imm -> ACExpr (CImmExpr imm)) in
    let check_const =
      match const with
      | CInt n -> CIf (ImmInt n, ImmIdentifier varname, ImmInt 0)
      | CBool b -> CIf (ImmBool b, ImmIdentifier varname, ImmBool false)
    in
    ALet (varname, check_const, anf_body)
;;

let anf_program (program : prog) : abinding list =
  reset_counter ();
  List.fold_right
    (fun decl acc ->
      match decl with
      | DLet (is_rec, pt, exp) ->
        let anf_exp = anf exp (fun imm -> ACExpr (CImmExpr imm)) in
        let id =
          match pt with
          | PtVar id -> id
          | PtWild -> fresh_var ()
          | PtConst _ -> fresh_var ()
        in
        AVal (id, anf_exp) :: acc)
    program
    []
;;

let debug_print_expr expr = Printf.printf "Debug Expression: %s\n" (show_exp expr)

let debug_print_abinding_list abinding_list =
  List.iter
    (fun abinding ->
      Printf.printf "Debug Expression: %s\n" (show_abinding abinding);
      flush stdout)
    abinding_list
;;

let debug_print_aexpr aexpr =
  Printf.printf "Debug ANF Expression: %s\n" (show_aexpr aexpr)
;;

let%test "anf_simple_constant" =
  let expr = [ DLet (false, PtVar "x", EConst (CInt 42)) ] in
  let expected = [ AVal ("x", ACExpr (CImmExpr (ImmInt 42))) ] in
  anf_program expr = expected
;;

let%test "anf_function_application" =
  let expr = [ DLet (false, PtVar "x", EApp (EVar "f", EConst (CInt 42))) ] in
  let expected =
    [ AVal
        ( "x"
        , ALet
            ( "anf_1"
            , CApp (ImmIdentifier "f", ImmInt 42)
            , ACExpr (CImmExpr (ImmIdentifier "anf_1")) ) )
    ]
  in
  anf_program expr = expected
;;

let%test "anf_factorial_test" =
  let expr =
    [ EFun (PtVar "fack1", EApp (EVar "k", EBinOp (Mul, EVar "m", EVar "n")))
    ; EFun (PtVar "var", EVar "x")
    ; EFun (PtVar "fac", EApp (EVar "n", EVar "var"))
    ]
    |> List.map (fun e -> DLet (false, PtVar "some_id", e))
  in
  let expected =
    [ AVal
        ( "some_id"
        , ALet
            ( "anf_4"
            , CImmExpr (ImmIdentifier "_")
            , ALet
                ( "anf_5"
                , CBinaryOp (Mul, ImmIdentifier "m", ImmIdentifier "n")
                , ALet
                    ( "anf_6"
                    , CApp (ImmIdentifier "k", ImmIdentifier "anf_5")
                    , ACExpr (CImmExpr (ImmIdentifier "anf_6")) ) ) ) )
    ; AVal
        ( "some_id"
        , ALet
            ("anf_3", CImmExpr (ImmIdentifier "_"), ACExpr (CImmExpr (ImmIdentifier "x")))
        )
    ; AVal
        ( "some_id"
        , ALet
            ( "anf_1"
            , CImmExpr (ImmIdentifier "_")
            , ALet
                ( "anf_2"
                , CApp (ImmIdentifier "n", ImmIdentifier "var")
                , ACExpr (CImmExpr (ImmIdentifier "anf_2")) ) ) )
    ]
  in
  debug_print_abinding_list expected;
  let result = anf_program expr in
  Printf.printf "Result length: %d\n" (List.length result);
  debug_print_abinding_list result;
  result = expected
;;

let%test "anf_simple_expression" =
  let expr = EConst (CInt 42) in
  debug_print_expr expr;
  let expected = ACExpr (CImmExpr (ImmInt 42)) in
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  debug_print_aexpr result;
  result = expected
;;

let%test "anf_unary_operation" =
  let expr = EUnOp (Minus, EConst (CInt 42)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = -42 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet (varname, CUnaryOp (Minus, ImmInt 42), ACExpr (CImmExpr (ImmIdentifier vn)))
    when varname = vn -> true
  | _ -> false
;;

let%test "anf_binary_operation" =
  let expr = EBinOp (Add, EConst (CInt 40), EConst (CInt 2)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = 40 + 2 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet (_, CBinaryOp (Add, ImmInt 40, ImmInt 2), ACExpr (CImmExpr (ImmIdentifier _))) ->
    true
  | _ -> false
;;

let%test "anf_conditional_expression" =
  let expr = EIf (EConst (CBool true), EConst (CInt 1), EConst (CInt 0)) in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let varname = if true then 1 else 0 in varname *)
  debug_print_aexpr result;
  match result with
  | ALet (_, CIf (ImmBool true, ImmInt 1, ImmInt 0), ACExpr (CImmExpr (ImmIdentifier _)))
    -> true
  | _ -> false
;;

let%test "anf_let_in_expression" =
  let expr = ELet ([ true, PtVar "x", EConst (CInt 42) ], EVar "x") in
  debug_print_expr expr;
  let result = anf expr (fun imm -> ACExpr (CImmExpr imm)) in
  (* Expected ANF form: let x = 42 in x *)
  debug_print_aexpr result;
  match result with
  | ALet ("x", CImmExpr (ImmInt 42), ACExpr (CImmExpr (ImmIdentifier "x"))) -> true
  | _ -> false
;;

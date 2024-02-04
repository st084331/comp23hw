(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llvm
open Result
open Ast
open Anf
open Ast_pretty

let context = global_context ()
let the_module = create_module context "ZRusML"
let builder = builder context
let int_type = i64_type context
let bool_type = i1_type context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 50
let ( let* ) = bind

let codegen_imm imm_val =
  let const_int_type x = ok (const_int int_type x) in
  let load_build v id = ok (build_load int_type v id builder) in
  let build_func_call f v =
    ok
      (build_call
         (function_type int_type [| int_type; int_type |])
         f
         [| build_pointercast v int_type "cast_pointer_to_int" builder
          ; params v |> Base.Array.length |> const_int int_type
         |]
         "part_applyc"
         builder)
  in
  let find_and_build id =
    match lookup_function id the_module with
    | Some v ->
      build_func_call (Option.get (lookup_function "create_new_apply" the_module)) v
    | None -> error (Format.sprintf "Unknown variable: %s" id)
  in
  let find_and_load id =
    match Hashtbl.find_opt named_values id with
    | Some v -> load_build v id
    | None -> find_and_build id
  in
  match imm_val with
  | ImmInt x -> const_int_type x
  | ImmBool b -> const_int_type (Base.Bool.to_int b)
  | ImmIdentifier id -> find_and_load id
;;

let codegen_binop binop =
  let build_op fn_builder nm_builder x y = fn_builder x y nm_builder builder in
  let get_constructor = function
    | Add -> build_add
    | Sub -> build_sub
    | Mul -> build_mul
    | Div -> build_sdiv
    | Eq -> build_icmp Icmp.Eq
    | Neq -> build_icmp Icmp.Ne
    | Less -> build_icmp Icmp.Slt
    | Leq -> build_icmp Icmp.Sle
    | Gre -> build_icmp Icmp.Sgt
    | Geq -> build_icmp Icmp.Sge
    | And -> build_and
    | Or -> build_or
  in
  build_op (get_constructor binop) (show_bin_op binop)
;;

let rec codegen_aexpr = function
  | ALet (id, c, ae) ->
    let* body = codegen_cexpr c in
    let alloca = build_alloca int_type id builder in
    let _ = build_store body alloca builder in
    Hashtbl.add named_values id alloca;
    codegen_aexpr ae
  | ACExpr c -> codegen_cexpr c

and codegen_cexpr = function
  | CUnaryOp (Not, x) ->
    let* x' = codegen_imm x in
    let res = build_not x' "not" builder in
    ok (build_trunc res bool_type "to_bool" builder)
  | CUnaryOp (Minus, x) ->
    let* x' = codegen_imm x in
    let res = build_neg x' "neg" builder in
    ok (build_zext res int_type "to_int" builder)
  | CBinaryOp (op, l, r) ->
    let* l' = codegen_imm l in
    let* r' = codegen_imm r in
    let res = codegen_binop op l' r' in
    ok (build_zext res int_type "to_int" builder)
  | CImmExpr e -> codegen_imm e
  | CApp (func, argument) ->
    let* calee = codegen_imm func in
    let* arg = codegen_imm argument in
    ok
      (build_call
         (function_type int_type [| int_type; int_type |])
         (Option.get (lookup_function "partially_apply" the_module))
         [| calee; arg |]
         "papply"
         builder)
  | CIf (cond, then_, else_) ->
    let* cond_val = codegen_imm cond in
    let zero = const_int int_type 0 in
    let cond_val = build_icmp Icmp.Ne cond_val zero "ifcondition" builder in
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then_br" the_function in
    position_at_end then_bb builder;
    let* then_val = codegen_aexpr then_ in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block context "else_br" the_function in
    position_at_end else_bb builder;
    let* else_val = codegen_aexpr else_ in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block context "ifcontext" the_function in
    position_at_end merge_bb builder;
    let phi =
      build_phi [ then_val, new_then_bb; else_val, new_else_bb ] "ifphi" builder
    in
    position_at_end start_bb builder;
    let _ = build_cond_br cond_val then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let _ = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let _ = build_br merge_bb builder in
    position_at_end merge_bb builder;
    ok phi
;;

let codegen_abind =
  let rec check_args acc = function
    | [] -> ok (List.rev acc)
    | PImmExpr (ImmIdentifier id) :: xs -> check_args (id :: acc) xs
    | PImmWild :: xs -> check_args ("0unused" :: acc) xs
    | _ -> error "Invalid argument"
  in
  let decl_or_err id ftype the_module =
    match lookup_function id the_module with
    | Some _ -> error "Function already exists"
    | None -> ok @@ declare_function id ftype the_module
  in
  let set_names names params =
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        set_value_name name a)
      params
  in
  let build_and_updt_values names params =
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        let alloca = build_alloca int_type name builder in
        let (_ : llvalue) = build_store a alloca builder in
        Hashtbl.add named_values name alloca)
      params
  in
  function
  | ABind (id, args, body) ->
    Hashtbl.clear named_values;
    let arg_names = check_args [] in
    let* names = arg_names args in
    let ints = Array.make (List.length args) int_type in
    let ftype = function_type int_type ints in
    let* func = decl_or_err id ftype the_module in
    set_names names (params func);
    let bb = append_block context "entry" func in
    position_at_end bb builder;
    build_and_updt_values names (params func);
    let* ret_val = codegen_aexpr body in
    let _ =
      if id = "main"
      then build_ret (const_int int_type 0) builder
      else build_ret ret_val builder
    in
    ok func
;;

let codegen_program prog =
  let runtime =
    [ declare_function
        "create_new_apply"
        (function_type int_type [| int_type; int_type |])
        the_module
    ; declare_function
        "partially_apply"
        (function_type int_type [| int_type; int_type |])
        the_module
    ; declare_function "print_int" (function_type int_type [| int_type |]) the_module
    ; declare_function "print_bool" (function_type int_type [| int_type |]) the_module
    ; declare_function "print_endline" (function_type int_type [||]) the_module
    ; declare_function "print_char" (function_type int_type [| int_type |]) the_module
    ]
  in
  let* result =
    List.fold_left
      (fun acc abind ->
        let* acc = acc in
        let* res = codegen_abind abind in
        ok (res :: acc))
      (ok runtime)
      prog
  in
  ok (List.rev result)
;;

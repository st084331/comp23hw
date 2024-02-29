(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llvm
open Result
open Ast
open Anf
open Parser
open Closure_conversion
open Lambda_lifting
open Ast_validator

let context = global_context ()
let the_module = create_module context "ZRusML"
let builder = builder context
let int_type = i64_type context
let bool_type = i1_type context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 50
let ( let* ) = bind

let codegen_imm = function
  | ImmInt x -> ok (const_int int_type x)
  | ImmBool b -> ok (const_int bool_type (Base.Bool.to_int b))
  | ImmIdentifier id ->
    (match Hashtbl.find_opt named_values id with
     | Some v -> ok (build_load int_type v id builder)
     | None ->
       (match lookup_function id the_module with
        | Some v ->
          ok
            (build_call
               (function_type int_type [| int_type; int_type |])
               (Option.get (lookup_function "addNewPAppliClosure" the_module))
               [| build_pointercast v int_type "cast_pointer_to_int" builder
                ; params v |> Base.Array.length |> const_int int_type
               |]
               "PAppliClosure"
               builder)
        | None -> error "Unknown variable"))
;;

let codegen_binop = function
  | Add -> fun x y -> build_add x y "addtmp" builder
  | Sub -> fun x y -> build_sub x y "subtmp" builder
  | Mul -> fun x y -> build_mul x y "multmp" builder
  | Div -> fun x y -> build_sdiv x y "divtmp" builder
  | Eq -> fun x y -> build_icmp Icmp.Eq x y "eqtmp" builder
  | Neq -> fun x y -> build_icmp Icmp.Ne x y "neqtmp" builder
  | Less -> fun x y -> build_icmp Icmp.Slt x y "lesstmp" builder
  | Leq -> fun x y -> build_icmp Icmp.Sle x y "leqtmp" builder
  | Gre -> fun x y -> build_icmp Icmp.Sgt x y "gretmp" builder
  | Geq -> fun x y -> build_icmp Icmp.Sge x y "geqtmp" builder
  | And -> fun x y -> build_and x y "andtmp" builder
  | Or -> fun x y -> build_or x y "ortmp" builder
;;

let codegen_unop = function
  | Not -> fun x -> build_not x "nottmp" builder
  | Minus -> fun x -> build_neg x "negtmp" builder
;;

let rec codegen_aexpr = function
  | ALet (id, c, ae) ->
    let* body = codegen_cexpr c in
    let alloca = build_alloca int_type id builder in
    let (_ : Llvm.llvalue) = build_store body alloca builder in
    Hashtbl.add named_values id alloca;
    codegen_aexpr ae
  | ACExpr c -> codegen_cexpr c

and codegen_cexpr = function
  | CUnaryOp (op, x) -> codegen_cexpr (CBinaryOp (Sub, ImmInt 0, x))
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
         (Option.get (lookup_function "applyPAppli" the_module))
         [| calee; arg |]
         "PAppliApplication"
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
    let (_ : Llvm.llvalue) = build_cond_br cond_val then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let (_ : Llvm.llvalue) = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let (_ : Llvm.llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    ok phi
;;

let codegen_abind = function
  | ABind (_, id, args, body) ->
    Hashtbl.clear named_values;
    let ints = Array.make (List.length args) int_type in
    let ftype = function_type int_type ints in
    let* func =
      match lookup_function id the_module with
      | Some _ -> error "Function already exists"
      | None -> ok @@ declare_function id ftype the_module
    in
    let* names =
      let rec check acc = function
        | [] -> ok (List.rev acc)
        | PImmExpr (ImmIdentifier id) :: xs -> check (id :: acc) xs
        | PImmWild :: xs -> check ("0_unused" :: acc) xs
        | _ -> error "Invalid argument"
      in
      check [] args
    in
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        set_value_name name a)
      (params func);
    let bb = append_block context "entry" func in
    position_at_end bb builder;
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        let alloca = build_alloca int_type name builder in
        let (_ : llvalue) = build_store a alloca builder in
        Hashtbl.add named_values name alloca)
      (params func);
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
        "addNewPAppliClosure"
        (function_type int_type [| int_type; int_type |])
        the_module
    ; declare_function
        "applyPAppli"
        (function_type int_type [| int_type; int_type |])
        the_module
    ; declare_function "print_int" (function_type int_type [| int_type |]) the_module
    ; declare_function "print_bool" (function_type int_type [| int_type |]) the_module
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

let print_prog_result code =
  match parse prog code with
  | Ok res ->
    let prog_closure = transform_decls res in
    let lifted = lift_prog prog_closure in
    let validated_prog = validate_prog lifted in
    let anf_prog = anf_program validated_prog in
    (match codegen_program anf_prog with
     | Ok llvalue_list ->
       Base.List.iter llvalue_list ~f:(fun f ->
         Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f))
     | Error e -> Stdlib.Format.printf "Error%s" e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;
(*
   let%expect_test "anf test sample" =
  let code =
    {|
    let fac n =
      let rec fack n k =
        if n <= 1 then 1
        else fack (n - 1) (fun m -> k (m * n))
      in
      fack n (fun x -> x)
    ;;

    let ans = fac 5;;
  |}
  in
  print_prog_result code;
  [%expect {|

  |}]
;;
*)

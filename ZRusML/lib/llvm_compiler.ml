open Llvm
open Ast

(* Initialize LLVM Context and Module *)
let context = global_context ()
let the_module = create_module context "ZRusML"
let builder = builder context

(* Define LLVM types corresponding to your language types *)
let int_type = i64_type context (* Assuming 64-bit integers *)
let bool_type = i1_type context (* Booleans *)
let named_values = Hashtbl.create 50 (* Variable environment *)

(* Utility functions for constants *)
let rec codegen_const = function
  | CInt i -> const_int int_type i
  | CBool b -> const_int bool_type (if b then 1 else 0)
;;

(* Codegen functions for expressions *)
let rec codegen_exp = function
  | EConst c -> codegen_const c
  | EUnOp (op, e) -> codegen_unop op e
  | EBinOp (op, e1, e2) -> codegen_binop op e1 e2
  | EVar id ->
    (try Hashtbl.find named_values id with
     | Not_found -> raise (Failure "Unknown variable name"))
  | ELet (bindings, e) ->
    let transformed_bindings =
      List.map
        (fun (is_rec, pattern, expr) ->
          match pattern with
          | PtVar id -> is_rec, id, expr
          | _ -> raise (Failure "Invalid pattern in let-binding"))
        bindings
    in
    let _ =
      List.fold_left
        (fun _ (is_rec, id, expr) ->
          let new_val = codegen_exp expr in
          Hashtbl.add named_values id new_val;
          new_val)
        (const_null int_type)
        transformed_bindings
    in
    codegen_exp e
  | EFun (pt, e) -> codegen_fun pt e
  | EIf (cond, e1, e2) -> codegen_if cond e1 e2
  | EApp (e1, e2) -> codegen_app e1 e2

and codegen_fun pt e =
  let return_type = int_type in
  let arg_types = [| int_type |] in
  let function_type = function_type return_type arg_types in
  let function_val = define_function "function_name" function_type the_module in
  let bb = append_block context "entry" function_val in
  position_at_end bb builder;
  (match pt with
   | PtVar id ->
     let arg = Array.get (params function_val) 0 in
     set_value_name id arg;
     let alloca = build_alloca int_type id builder in
     ignore (build_store arg alloca builder);
     Hashtbl.add named_values id alloca
   | _ -> raise (Failure "Invalid function parameter pattern"));
  codegen_exp e

and codegen_if cond e1 e2 =
  let cond_val = codegen_exp cond in
  let zero = const_int int_type 0 in
  let cond_val = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
  let start_bb = insertion_block builder in
  let the_function = block_parent start_bb in
  let then_bb = append_block context "then" the_function in
  let else_bb = append_block context "else" the_function in
  let merge_bb = append_block context "ifcont" the_function in
  position_at_end then_bb builder;
  let then_val = codegen_exp e1 in
  ignore (build_br merge_bb builder);
  position_at_end else_bb builder;
  let else_val = codegen_exp e2 in
  ignore (build_br merge_bb builder);
  position_at_end merge_bb builder;
  let phi = build_phi [ then_val, then_bb; else_val, else_bb ] "iftmp" builder in
  position_at_end start_bb builder;
  ignore (build_cond_br cond_val then_bb else_bb builder);
  phi

and codegen_app e1 e2 =
  let calee = codegen_exp e1 in
  let arg = codegen_exp e2 in
  build_call
    (function_type int_type [| int_type; int_type |])
    (Option.get (lookup_function "applyPAppli" the_module))
    [| calee; arg |]
    "PAppliApplication"
    builder

and codegen_binop op e1 e2 =
  let e1_ir = codegen_exp e1 in
  let e2_ir = codegen_exp e2 in
  match op with
  | And -> build_and e1_ir e2_ir "andtmp" builder
  | Or -> build_or e1_ir e2_ir "ortmp" builder
  | Less -> build_icmp Icmp.Slt e1_ir e2_ir "lesstmp" builder
  | Leq -> build_icmp Icmp.Sle e1_ir e2_ir "leqtmp" builder
  | Gre -> build_icmp Icmp.Sgt e1_ir e2_ir "gretmp" builder
  | Geq -> build_icmp Icmp.Sge e1_ir e2_ir "geqtmp" builder
  | Eq -> build_icmp Icmp.Eq e1_ir e2_ir "eqtmp" builder
  | Neq -> build_icmp Icmp.Ne e1_ir e2_ir "neqtmp" builder
  | Add -> build_add e1_ir e2_ir "addtmp" builder
  | Sub -> build_sub e1_ir e2_ir "subtmp" builder
  | Mul -> build_mul e1_ir e2_ir "multmp" builder
  | Div -> build_sdiv e1_ir e2_ir "divtmp" builder

and codegen_unop op e =
  let e_ir = codegen_exp e in
  match op with
  | Not -> build_not e_ir "nottmp" builder
  | Minus -> build_neg e_ir "negtmp" builder
;;

(* Main compilation function *)
let compile ast =
  List.iter (fun exp -> ignore (codegen_exp exp)) ast;
  the_module
;;

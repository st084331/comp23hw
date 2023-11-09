open Base
open Ast
open Cc_ast
open Counter

let gen_var = gen_var "cc"

let simplify program =
  let rec uncurry acc = function
    | EAbs (arg, body) -> uncurry (arg :: acc) body
    | expr -> CAbs (List.rev acc, decompose_letin_expr expr)
  and decompose_letin_expr = function
    | EIdentifier x -> CIdentifier x
    | ELiteral x -> CLiteral x
    | EUnaryOp (op, body) -> CUnaryOp (op, decompose_letin_expr body)
    | EBinaryOp (op, l, r) ->
      CBinaryOp (op, decompose_letin_expr l, decompose_letin_expr r)
    | EIfThenElse (c, t, e) ->
      CIfThenElse (decompose_letin_expr c, decompose_letin_expr t, decompose_letin_expr e)
    | EApp (l, r) -> CApp (decompose_letin_expr l, decompose_letin_expr r)
    | EAbs (arg, body) -> uncurry [ arg ] body
    | ELetIn (bindings, body) ->
      (match bindings with
       | [ b ] -> CLetIn (decompose_letin_bind b, decompose_letin_expr body)
       | h :: tl ->
         CLetIn (decompose_letin_bind h, decompose_letin_expr (e_let_in tl body))
       | [] -> failwith "Unreachable")
  and decompose_letin_bind = function
    | BVal (name, body) -> CVal (name, decompose_letin_expr body)
    | BFun (name, args, body) -> CFun (name, args, decompose_letin_expr body)
  in
  List.map program ~f:decompose_letin_bind
;;

let rec free_vars = function
  | CIdentifier var -> Set.singleton (module String) var
  | CUnaryOp (_, body) -> free_vars body
  | CBinaryOp (_, left, right) | CApp (left, right) ->
    Set.union (free_vars left) (free_vars right)
  | CIfThenElse (cond, if_true, if_false) ->
    Set.union_list
      (module String)
      [ free_vars cond; free_vars if_true; free_vars if_false ]
  | _ -> Set.empty (module String)
;;

let create_closure closed_args args body =
  let name = gen_var () in
  CFun (name, List.append closed_args args, body)
;;

let apply_closure closure args =
  let id =
    match closure with
    | CFun (varname, _, _) | CVal (varname, _) -> varname
  in
  let rec rev_partial_app = function
    | [ a ] -> CIdentifier a
    | h :: t -> CApp (rev_partial_app t, CIdentifier h)
    | [] -> CIdentifier id
  in
  rev_partial_app (List.rev (id :: args))
;;

let get_function_names env =
  let get_name = function
    | CFun (varname, _, _) | CVal (varname, _) -> varname
  in
  Set.of_list (module String) (List.map env ~f:get_name)
;;

let rec conv_closure_binding env = function
  | CVal (id, body) ->
    let cc_body, env' = conv_closure_expr env body in
    CVal (id, cc_body), env'
  | CFun _ -> failwith ""

and conv_closure_expr env = function
  | CIdentifier x -> CIdentifier x, env
  | CLiteral x -> CLiteral x, env
  | CUnaryOp (op, body) ->
    let cc_body, env' = conv_closure_expr env body in
    CUnaryOp (op, cc_body), env'
  | CBinaryOp (op, left, right) ->
    let cc_left, env' = conv_closure_expr env left in
    let cc_right, env'' = conv_closure_expr env' right in
    CBinaryOp (op, cc_left, cc_right), env''
  | CIfThenElse (cond, if_true, if_false) ->
    let cc_cond, env' = conv_closure_expr env cond in
    let cc_if_true, env'' = conv_closure_expr env' if_true in
    let cc_if_false, env''' = conv_closure_expr env'' if_false in
    CIfThenElse (cc_cond, cc_if_true, cc_if_false), env'''
  | CApp (left, right) ->
    let cc_left, env' = conv_closure_expr env left in
    let cc_right, env'' = conv_closure_expr env' right in
    CApp (cc_left, cc_right), env''
  | CAbs (args, body) ->
    let cc_body, env' = conv_closure_expr env body in
    let closed_args = Set.diff (free_vars cc_body) (get_function_names env') in
    let cc_abs, env'' =
      match Set.to_list closed_args with
      | [] -> CAbs (args, cc_body), env'
      | closed_args ->
        let closure = create_closure closed_args args cc_body in
        let env'' = closure :: env' in
        let cc_abs = apply_closure closure closed_args in
        cc_abs, env''
    in
    cc_abs, env''
  | CLetIn (binding, body) ->
    let cc_binding, env' = conv_closure_binding env binding in
    let cc_letin_body, env'' = conv_closure_expr env' body in
    CLetIn (cc_binding, cc_letin_body), env''
;;

(*
   val res = let fun f x y = (fn z => x + y + z) 1 in f end

   fun cc_1 x y z = x + y + z
   val res = let fun f x y = (cc1 x y) 1 in f end

   val res = let fun f x y =
   let fun g z = x + y + z
   in g 1
   in f end

   fun g x y z = x + y + z
   val res = let fun f x y = g 1
   in f end

   val res = let fun f x y =
   let fun g z = (fn w => g w) x
   in g 1
   in f end

   fun cc_1 g w = g w
   fun g x y z = cc_1 g
   val res = let fun f x y = g 1
   in f end
*)

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
      List.fold_right
        bindings
        ~f:(fun b1 b2 -> CLetIn (decompose_letin_bind b1, b2))
        ~init:(decompose_letin_expr body)
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
  List.fold_left args ~f:(fun l r -> CApp (l, CIdentifier r)) ~init:(CIdentifier id)
;;

let get_function_names env =
  let get_name = function
    | CFun (varname, _, _) | CVal (varname, _) -> varname
  in
  Set.of_list (module String) (List.map env ~f:get_name)
;;

let replace_id_by_expr where id expr =
  match where with
  | CIdentifier x when id == x -> expr
  | e -> e
;;

let rec clos_conv env parent_scope = function
  | CIdentifier x -> CIdentifier x, env
  | CLiteral x -> CLiteral x, env
  | CUnaryOp (op, body) ->
    let cc_body, env' = clos_conv env parent_scope body in
    CUnaryOp (op, cc_body), env'
  | CBinaryOp (op, left, right) ->
    let cc_left, env' = clos_conv env parent_scope left in
    let cc_right, env'' = clos_conv env' parent_scope right in
    CBinaryOp (op, cc_left, cc_right), env''
  | CIfThenElse (cond, if_true, if_false) ->
    let cc_cond, env' = clos_conv env parent_scope cond in
    let cc_if_true, env'' = clos_conv env' parent_scope if_true in
    let cc_if_false, env''' = clos_conv env'' parent_scope if_false in
    CIfThenElse (cc_cond, cc_if_true, cc_if_false), env'''
  | CApp (left, right) ->
    let cc_left, env' = clos_conv env parent_scope left in
    let cc_right, env'' = clos_conv env' parent_scope right in
    CApp (cc_left, cc_right), env''
  | CAbs (args, body) ->
    let cc_body, env' = clos_conv env parent_scope body in
    let closed_args = Set.diff (free_vars cc_body) (get_function_names env') in
    let cc_abs, env'' =
      match Set.to_list closed_args with
      | [] -> CAbs (args, cc_body), env'
      | closed_args ->
        let closure = create_closure closed_args args cc_body in
        apply_closure closure closed_args, closure :: env'
    in
    cc_abs, env''
  | CLetIn (binding, letin_body) ->
    (match binding with
     | CVal (id, body) ->
       let cc_body, env' = clos_conv env parent_scope body in
       let closed_args =
         Set.diff
           (free_vars cc_body)
           (Set.union
              (Set.of_list (module String) parent_scope)
              (get_function_names env'))
       in
       (match Set.to_list closed_args with
        | [] ->
          let cc_letin_body, env'' = clos_conv env' (id :: parent_scope) letin_body in
          CLetIn (CVal (id, cc_body), cc_letin_body), env''
        | closed_args ->
          let closure = create_closure closed_args [] cc_body in
          let applied_closure = apply_closure closure closed_args in
          let cc_body = replace_id_by_expr cc_body id applied_closure in
          let cc_letin_body, env'' =
            clos_conv (closure :: env') (id :: parent_scope) letin_body
          in
          let cc_letin_body = replace_id_by_expr cc_letin_body id applied_closure in
          CLetIn (CFun (id, closed_args, cc_body), cc_letin_body), env'')
     | CFun (id, args, body) ->
       let cc_body, env' = clos_conv env parent_scope body in
       let closed_args =
         Set.diff
           (free_vars cc_body)
           (Set.union
              (Set.of_list (module String) parent_scope)
              (Set.add (get_function_names env') id))
       in
       (match Set.to_list closed_args with
        | [] ->
          let cc_letin_body, env'' = clos_conv env' (id :: parent_scope) letin_body in
          CLetIn (CFun (id, args, cc_body), cc_letin_body), env''
        | closed_args ->
          let closure = create_closure closed_args args cc_body in
          let applied_closure = apply_closure closure closed_args in
          let cc_body = replace_id_by_expr cc_body id applied_closure in
          let cc_letin_body, env'' =
            clos_conv (closure :: env') (id :: parent_scope) letin_body
          in
          let cc_letin_body = replace_id_by_expr cc_letin_body id applied_closure in
          CLetIn (CFun (id, List.append closed_args args, cc_body), cc_letin_body), env''))
;;

let get_name = function
  | CVal (id, _) | CFun (id, _, _) -> id
;;

let cc_program program =
  let rec helper acc parent_scope = function
    | [] -> acc
    | CVal (id, body) :: tl ->
      let cc_body, env = clos_conv [] parent_scope body in
      helper
        (CVal (id, List.fold_right env ~f:(fun l r -> CLetIn (l, r)) ~init:cc_body) :: acc)
        (id :: parent_scope)
        tl
    | CFun (id, args, body) :: tl ->
      let cc_body, env = clos_conv [] (id :: parent_scope) body in
      helper
        (CFun (id, args, List.fold_right env ~f:(fun l r -> CLetIn (l, r)) ~init:cc_body)
         :: acc)
        (id :: parent_scope)
        tl
  in
  List.rev (helper [] [] (simplify program))
;;

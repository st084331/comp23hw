open Ast
open Base
open Parser

let rec free_vars = function
  | EConst _ -> Set.empty (module String)
  | EVar x -> Set.singleton (module String) x
  | EBinOp (_, e1, e2) -> Set.union (free_vars e1) (free_vars e2)
  | EIf (e1, e2, e3) -> Set.union (Set.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | EApp (e1, e2) -> Set.union (free_vars e1) (free_vars e2)
  | EFun (x, e) ->
    let free = free_vars e in
    (match x with
     | PVar id -> Set.remove free id
     | _ -> free)
  | ELetIn (b, x, e1, e2) ->
    let free1 = free_vars e1 in
    let free2 = free_vars e2 in
    let free1' = if b then Set.remove free1 x else free1 in
    Set.union free1' (Set.remove free2 x)
;;

let closure_conversion global_env decl =
  let rec expr_closure local_env global_env = function
    | EConst x -> constr_econst x
    | EVar x as orig ->
      (match Map.find local_env x with
       | Some free ->
         Set.fold
           ~init:orig
           ~f:(fun expr x -> constr_eapp expr [ constr_evar x ])
           free
       | None -> orig)
    | EBinOp (op, e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_ebinop op e1' e2'
    | EIf (e1, e2, e3) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      let e3' = expr_closure local_env global_env e3 in
      constr_eif e1' e2' e3'
    | EApp (e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_eapp e1' [ e2' ]
    | EFun (x, _) as orig ->
      let s = free_vars orig in
      let e' = efun_helper local_env global_env orig in
      (match x with
       | PVar _ ->
         let fun_fold =
           Set.fold_right ~init:e' ~f:(fun x acc -> constr_efun [ constr_pvar x ] acc) s
         in
         let app_fold =
           Set.fold ~init:fun_fold ~f:(fun acc x -> constr_eapp acc [ constr_evar x ]) s
         in
         app_fold
       | _ -> e')
    | ELetIn (b, x, (EFun _ as e1), e2) as orig ->
      let free = free_vars orig in
      let free' = Set.diff free global_env in
      let e1' = efun_helper local_env global_env e1 in
      let local_env' = Map.set local_env ~key:x ~data:free' in
      let e2' = expr_closure local_env' (Set.add global_env x) e2 in
      constr_eletin b x e1' e2'
    | ELetIn (b, x, e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_eletin b x e1' e2'
  and efun_helper local_env global_env = function
    | EFun (x, e) ->
      (match x with
       | PVar _ as orig ->
         let e' = efun_helper local_env global_env e in
         constr_efun [ orig ] e'
       | p ->
         let e' = efun_helper local_env global_env e in
         constr_efun [ p ] e')
    | expr -> expr_closure local_env global_env expr
  in
  let decl_closure global_env = function
    | ELet (is_rec, id, e) ->
      constr_elet is_rec id (expr_closure (Map.empty (module Base.String)) global_env e)
  in
  decl_closure global_env decl
;;

let prog_conversion = List.map ~f:closure_conversion

let print_prog_result decl =
  let buf = closure_conversion (Set.empty (module Base.String)) decl in
  Stdlib.Format.printf "%s" (Ast.show_program [ buf ])
;;

let%expect_test _ =
  print_prog_result
    (ELet
       ( false
       , "fac"
       , EFun
           ( PVar "n"
           , ELetIn
               ( true
               , "fack"
               , EFun
                   ( PVar "n"
                   , EFun
                       ( PVar "k"
                       , EIf
                           ( EBinOp (Leq, EVar "n", EConst (CInt 1))
                           , EApp (EVar "k", EConst (CInt 1))
                           , EApp
                               ( EApp
                                   (EVar "fack", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                               , EFun
                                   ( PVar "m"
                                   , EApp (EVar "k", EBinOp (Mul, EVar "m", EVar "n")) )
                               ) ) ) )
               , EApp (EApp (EVar "fack", EVar "n"), EFun (PVar "x", EVar "x")) ) ) ));
  [%expect
    {|
      [(ELet (false, "fac",
          (EFun ((PVar "n"),
             (ELetIn (true, "fack",
                (EFun ((PVar "n"),
                   (EFun ((PVar "k"),
                      (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
                         (EApp ((EVar "k"), (EConst (CInt 1)))),
                         (EApp (
                            (EApp ((EVar "fack"),
                               (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                            (EApp (
                               (EApp (
                                  (EFun ((PVar "k"),
                                     (EFun ((PVar "n"),
                                        (EFun ((PVar "m"),
                                           (EApp ((EVar "k"),
                                              (EBinOp (Mul, (EVar "m"), (EVar "n")
                                                 ))
                                              ))
                                           ))
                                        ))
                                     )),
                                  (EVar "k"))),
                               (EVar "n")))
                            ))
                         ))
                      ))
                   )),
                (EApp ((EApp ((EVar "fack"), (EVar "n"))),
                   (EFun ((PVar "x"), (EVar "x")))))
                ))
             ))
          ))
        ] |}]
;;

(*
   let%expect_test _ =
   print_prog_result
   (ELet
   ( false
   , "a"
   , EFun
   ( PVar "c"
   , ELetIn
   ( false
   , "k"
   , EFun (PVar "m", EBinOp (Add, EVar "m", EVar "c"))
   , EApp (EVar "k", EConst (CInt 5)) ) ) ));
   [%expect
    {|
      [(ELet (false, "a",
          (EFun ((PVar "c"),
             (ELetIn (false, "k",
                (EFun ((PVar "m"),
                   (EApp (
                      (EFun ((PVar "c"), (EBinOp (Add, (EVar "m"), (EVar "c"))))),
                      (EVar "c")))
                   )),
                (EApp ((EVar "k"), (EConst (CInt 5))))))
             ))
          ))
        ]
     |}]
   ;; *)

(* let fac n =
   let rec fack n k =
   if n <= 1 then k 1 else fack (n - 1) ((fun k n m -> k (m * n)) k n)
   in
   fack n (fun x -> x)
   ;;

   let fac n =
   let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n)) in
   fack n (fun x -> x)
   ;; *)

(* let fac n =
   let fack1 k m = k (m * n) in
   let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fack1 k ) in
   fack n (fun x -> x);; *)

let a c d =
  let m = c + d in
  let k _ = 1 + m in
  k (5 + m)
;;

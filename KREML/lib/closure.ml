open Ast
open Cc_ast


let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EIfThenElse
            ( EBinaryOp (GtOrEq, EIdentifier "y", ELiteral (LInt 4))
            , EAbs ("z", EBinaryOp (Add, EIdentifier "z", ELiteral (LInt 1)))
            , EIdentifier "f" ) )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CIfThenElse
            ( CBinaryOp (GtOrEq, CIdentifier "y", CLiteral (LInt 4))
            , CAbs ([ "z" ], CBinaryOp (Add, CIdentifier "z", CLiteral (LInt 1)))
            , CIdentifier "f" ) )
    ]
  in
  compare (сс_program ast) сс
;;

let%test _ =
  let ast =
    [ BVal ("y", ELetIn ([ BVal ("x", ELiteral (LInt 5)) ], EIdentifier "x")) ]
  in
  let cc = [ CVal ("y", CLetIn (CVal ("x", CLiteral (LInt 5)), CIdentifier "x")) ] in
  compare (сс_program ast) сс
;;

6.

let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EIfThenElse
            ( EBinaryOp (LtOrEq, EIdentifier "y", ELiteral (LInt 4))
            , EAbs ("z", EBinaryOp (Add, EIdentifier "z", ELiteral (LInt 1)))
            , EIdentifier "f" ) )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CIfThenElse
            ( CBinaryOp (LtOrEq, CIdentifier "y", CLiteral (LInt 4))
            , CAbs ([ "z" ], CBinaryOp (Add, CIdentifier "z", CLiteral (LInt 1)))
            , CIdentifier "f" ) )
    ]
  in
  compare (сс_program ast) сс
;;

10.

let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EBinaryOp
            (And, EBinaryOp (Or, EIdentifier "a", EIdentifier "b"), EIdentifier "c") )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CBinaryOp
            (And, CBinaryOp (Or, CIdentifier "a", CIdentifier "b"), CIdentifier "c") )
    ]
  in
  compare (сс_program ast) сс
;;

let%test _ =
  let ast =
    [ BVal ("x", EApp (EApp (EIdentifier "f", EIdentifier "a"), EIdentifier "b")) ]
  in
  let cc =
    [ CVal ("x", CApp (CApp (CIdentifier "f", CIdentifier "a"), CIdentifier "b")) ]
  in
  compare (сс_program ast) сс
;;

let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EIfThenElse
            ( EUnaryOp (Not, EBinaryOp (Eq, EIdentifier "a", EIdentifier "b"))
            , EIdentifier "c"
            , EIdentifier "d" ) )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CIfThenElse
            ( CUnaryOp (Not, CBinaryOp (Eq, CIdentifier "a", CIdentifier "b"))
            , CIdentifier "c"
            , CIdentifier "d" ) )
    ]
  in
  compare (сс_program ast) сс
;;



(*   
 fun fac n =
      let fun fack n k =
        if n <= 1 then k 1
        else fack (n-1) (fn m => k (m * n))
      in
        fack n (fn x => x)
      end

    -> cc ->

    fun fac n =
      let fun fack1 k n m = k (m * n) 
      in
      let fun fack n k =
        if n <= 1 then k 1
        else fack (n-1) (fack1 k n)
      in
        fack n (fn x => x)
      end
      end
*)
let%test _ = 
let ast = 
  [
    BFun(
      "fac",
      ["n"],
      ELetIn(
        [
          BFun(
            "fack",
            ["n"; "k"],
            EIfThenElse(
              EBinaryOp(LtOrEq, EIdentifier("n"), ELiteral(LInt(1))),
              EApp(EIdentifier("k"), ELiteral(LInt(1))),
              EApp(
                EApp(
                  EIdentifier("fack"),
                  EBinaryOp(Sub, EIdentifier("n"), ELiteral(LInt(1)))
                ),
                EAbs(
                  "m",
                  EApp(EIdentifier("k"),
                    EBinaryOp(Mult, EIdentifier("m"), EIdentifier("n"))
                  )
                )
              )
            )
          )
        ],
        EApp(EIdentifier("fack"),
          EApp (EIdentifier "n", EAbs ("x", EIdentifier "x"))
        )
      )
    )] in
let cc =   [ CFun
( "fac"
, [ "n" ]
, CLetIn
    ( CFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , CApp
            (CIdentifier "k", CBinaryOp (Mult, CIdentifier "m", CIdentifier "n"))
        )
    , CLetIn
        ( CFun
            ( "fack"
            , [ "n"; "k" ]
            , CIfThenElse
                ( CBinaryOp (LtOrEq, CIdentifier "n", CLiteral (LInt 1))
                , CApp (CIdentifier "k", CLiteral (LInt 1))
                , CApp
                    ( CApp
                        ( CIdentifier "fack"
                        , CBinaryOp (Sub, CIdentifier "n", CLiteral (LInt 1)) )
                    , CApp
                        ( CApp (CIdentifier "fack1", CIdentifier "k")
                        , CIdentifier "n" ) ) ) )
        , CApp
            ( CApp (CIdentifier "fack", CIdentifier "n")
            , CAbs ([ "x" ], CIdentifier "x") ) ) ) )
]
in compare (сс_program ast) сс

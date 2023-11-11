  $ ./demoAnf.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > EOF
  (AFun
     ( "cc_1"
     , [ "k"; "n"; "m" ]
     , ALet
         ( "anf_1"
         , CBinaryOp (Mult, ImmIdentifier "m", ImmIdentifier "n")
         , ALet
             ( "anf_2"
             , CApp (ImmIdentifier "k", ImmIdentifier "anf_1")
             , ACExpr (CImmExpr (ImmIdentifier "anf_2")) ) ) ))
    (AFun
       ( "fack"
       , [ "n"; "k" ]
       , ALet
           ( "anf_3"
           , CBinaryOp (LtOrEq, ImmIdentifier "n", ImmInt 1)
           , ALet
               ( "anf_4"
               , CApp (ImmIdentifier "k", ImmInt 1)
               , ALet
                   ( "anf_5"
                   , CBinaryOp (Sub, ImmIdentifier "n", ImmInt 1)
                   , ALet
                       ( "anf_6"
                       , CApp (ImmIdentifier "fack", ImmIdentifier "anf_5")
                       , ALet
                           ( "anf_7"
                           , CApp (ImmIdentifier "cc_1", ImmIdentifier "k")
                           , ALet
                               ( "anf_8"
                               , CApp (ImmIdentifier "anf_7", ImmIdentifier "n")
                               , ALet
                                   ( "anf_9"
                                   , CApp (ImmIdentifier "anf_6", ImmIdentifier "anf_8")
                                   , ALet
                                       ( "anf_10"
                                       , CIfThenElse
                                           ( ImmIdentifier "anf_3"
                                           , ImmIdentifier "anf_4"
                                           , ImmIdentifier "anf_9" )
                                       , ACExpr (CImmExpr (ImmIdentifier "anf_10")) ) ) )
                           ) ) ) ) ) ))
    (AFun ("ll_1", [ "x" ], ACExpr (CImmExpr (ImmIdentifier "x"))))
    (AFun
       ( "fac"
       , [ "n" ]
       , ALet
           ( "anf_10"
           , CApp (ImmIdentifier "fack", ImmIdentifier "n")
           , ALet
               ( "anf_11"
               , CApp (ImmIdentifier "anf_10", ImmIdentifier "ll_1")
               , ACExpr (CImmExpr (ImmIdentifier "anf_11")) ) ) ))
  int -> int

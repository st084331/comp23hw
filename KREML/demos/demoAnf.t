  $ ./demoAnf.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > EOF
    (AFun ("cc_1", ["k"; "n"; "m"],
     (ALet ("anf_9",
        (CBinaryOp (Mult, (ImmIdentifier "m"), (ImmIdentifier "n"))),
        (ALet ("anf_10", (CApp ((ImmIdentifier "k"), (ImmIdentifier "anf_9"))),
           (ACExpr (CImmExpr (ImmIdentifier "anf_10")))))
        ))
     ))
  (AFun ("fack", ["k"; "m"; "n"],
     (ALet ("anf_1", (CBinaryOp (LtOrEq, (ImmIdentifier "n"), (ImmInt 1))),
        (ALet ("anf_2", (CApp ((ImmIdentifier "k"), (ImmInt 1))),
           (ALet ("anf_3", (CBinaryOp (Sub, (ImmIdentifier "n"), (ImmInt 1))),
              (ALet ("anf_4",
                 (CApp ((ImmIdentifier "fack"), (ImmIdentifier "anf_3"))),
                 (ALet ("anf_5",
                    (CApp ((ImmIdentifier "cc_1"), (ImmIdentifier "k"))),
                    (ALet ("anf_6",
                       (CApp ((ImmIdentifier "anf_5"), (ImmIdentifier "m"))),
                       (ALet ("anf_7",
                          (CApp ((ImmIdentifier "anf_6"), (ImmIdentifier "n"))),
                          (ALet ("anf_8",
                             (CApp ((ImmIdentifier "anf_4"),
                                (ImmIdentifier "anf_7"))),
                             (ACExpr
                                (CIfThenElse ((ImmIdentifier "anf_1"),
                                   (ImmIdentifier "anf_2"),
                                   (ImmIdentifier "anf_8"))))
                             ))
                          ))
                       ))
                    ))
                 ))
              ))
           ))
        ))
     ))
  (AFun ("ll_1", ["x"], (ACExpr (CImmExpr (ImmIdentifier "x")))))
  (AFun ("fac", ["n"],
     (ALet ("anf_19", (CApp ((ImmIdentifier "fack"), (ImmIdentifier "n"))),
        (ALet ("anf_20", (CApp ((ImmIdentifier "cc_2"), (ImmIdentifier "x"))),
           (ALet ("anf_21",
              (CApp ((ImmIdentifier "anf_19"), (ImmIdentifier "anf_20"))),
              (ACExpr (CImmExpr (ImmIdentifier "anf_21")))))
           ))
        ))
     ))
  int -> int

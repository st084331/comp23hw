declare i64 @print_bool(i64)

declare i64 @print_int(i64)

declare i64 @applyPAppli(i64, i64)

declare i64 @addNewPAppliClosure(i64, i64)

define i64 @ast_0(i64 %ast_1, i64 %ast_2, i64 %ast_3) {
entry:
  %ast_11 = alloca i64, align 8
  store i64 %ast_1, ptr %ast_11, align 4
  %ast_22 = alloca i64, align 8
  store i64 %ast_2, ptr %ast_22, align 4
  %ast_33 = alloca i64, align 8
  store i64 %ast_3, ptr %ast_33, align 4
  %ast_34 = load i64, ptr %ast_33, align 4
  %ast_25 = load i64, ptr %ast_22, align 4
  %multmp = mul i64 %ast_34, %ast_25
  %anf_12 = alloca i64, align 8
  store i64 %multmp, ptr %anf_12, align 4
  %ast_16 = load i64, ptr %ast_11, align 4
  %anf_127 = load i64, ptr %anf_12, align 4
  %PAppliApplication = call i64 @applyPAppli(i64 %ast_16, i64 %anf_127)
  %anf_13 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_13, align 4
  %anf_138 = load i64, ptr %anf_13, align 4
  ret i64 %anf_138
}

define i64 @ast_4(i64 %ast_5, i64 %ast_6) {
entry:
  %ast_51 = alloca i64, align 8
  store i64 %ast_5, ptr %ast_51, align 4
  %ast_62 = alloca i64, align 8
  store i64 %ast_6, ptr %ast_62, align 4
  %ast_53 = load i64, ptr %ast_51, align 4
  %leqtmp = icmp sle i64 %ast_53, 1
  %to_int = zext i1 %leqtmp to i64
  %anf_5 = alloca i64, align 8
  store i64 %to_int, ptr %anf_5, align 4
  %ast_54 = load i64, ptr %ast_51, align 4
  %subtmp = sub i64 %ast_54, 1
  %anf_6 = alloca i64, align 8
  store i64 %subtmp, ptr %anf_6, align 4
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_4 to i64), i64 2)
  %anf_65 = load i64, ptr %anf_6, align 4
  %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 %anf_65)
  %anf_7 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_7, align 4
  %PAppliClosure6 = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_0 to i64), i64 3)
  %ast_67 = load i64, ptr %ast_62, align 4
  %PAppliApplication8 = call i64 @applyPAppli(i64 %PAppliClosure6, i64 %ast_67)
  %anf_8 = alloca i64, align 8
  store i64 %PAppliApplication8, ptr %anf_8, align 4
  %anf_89 = load i64, ptr %anf_8, align 4
  %ast_510 = load i64, ptr %ast_51, align 4
  %PAppliApplication11 = call i64 @applyPAppli(i64 %anf_89, i64 %ast_510)
  %anf_9 = alloca i64, align 8
  store i64 %PAppliApplication11, ptr %anf_9, align 4
  %anf_712 = load i64, ptr %anf_7, align 4
  %anf_913 = load i64, ptr %anf_9, align 4
  %PAppliApplication14 = call i64 @applyPAppli(i64 %anf_712, i64 %anf_913)
  %anf_10 = alloca i64, align 8
  store i64 %PAppliApplication14, ptr %anf_10, align 4
  %anf_515 = load i64, ptr %anf_5, align 4
  %ifcondition = icmp ne i64 %anf_515, 0
  br i1 %ifcondition, label %then_br, label %else_br

then_br:                                          ; preds = %entry
  br label %ifcontext

else_br:                                          ; preds = %entry
  %anf_1016 = load i64, ptr %anf_10, align 4
  br label %ifcontext

ifcontext:                                        ; preds = %else_br, %then_br
  %ifphi = phi i64 [ 1, %then_br ], [ %anf_1016, %else_br ]
  %anf_11 = alloca i64, align 8
  store i64 %ifphi, ptr %anf_11, align 4
  %anf_1117 = load i64, ptr %anf_11, align 4
  ret i64 %anf_1117
}

define i64 @ast_7(i64 %ast_8) {
entry:
  %ast_81 = alloca i64, align 8
  store i64 %ast_8, ptr %ast_81, align 4
  %ast_82 = load i64, ptr %ast_81, align 4
  ret i64 %ast_82
}

define i64 @ast_9(i64 %ast_10) {
entry:
  %ast_101 = alloca i64, align 8
  store i64 %ast_10, ptr %ast_101, align 4
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_4 to i64), i64 2)
  %ast_11 = alloca i64, align 8
  store i64 %PAppliClosure, ptr %ast_11, align 4
  %ast_112 = load i64, ptr %ast_11, align 4
  %ast_103 = load i64, ptr %ast_101, align 4
  %PAppliApplication = call i64 @applyPAppli(i64 %ast_112, i64 %ast_103)
  %anf_3 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_3, align 4
  %anf_34 = load i64, ptr %anf_3, align 4
  %PAppliClosure5 = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_7 to i64), i64 1)
  %PAppliApplication6 = call i64 @applyPAppli(i64 %anf_34, i64 %PAppliClosure5)
  %anf_4 = alloca i64, align 8
  store i64 %PAppliApplication6, ptr %anf_4, align 4
  %anf_47 = load i64, ptr %anf_4, align 4
  ret i64 %anf_47
}

define i64 @ast_12() {
entry:
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_9 to i64), i64 1)
  ret i64 %PAppliClosure
}

define i64 @ast_13() {
entry:
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_12 to i64), i64 0)
  %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 5)
  %anf_1 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_1, align 4
  %anf_11 = load i64, ptr %anf_1, align 4
  ret i64 %anf_11
}

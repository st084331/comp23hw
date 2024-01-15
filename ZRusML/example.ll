declare i64 @print_bool(i64)

declare i64 @print_int(i64)

declare i64 @applyPAppli(i64, i64)

declare i64 @addNewPAppliClosure(i64, i64)

define i64 @ast_0(i64 %ast_1) {
entry:
  %ast_11 = alloca i64, align 8
  store i64 %ast_1, ptr %ast_11, align 4
  %ast_12 = load i64, ptr %ast_11, align 4
  %leqtmp = icmp sle i64 %ast_12, 1
  %to_int = zext i1 %leqtmp to i64
  %anf_3 = alloca i64, align 8
  store i64 %to_int, ptr %anf_3, align 4
  %anf_33 = load i64, ptr %anf_3, align 4
  %ifcondition = icmp ne i64 %anf_33, 0
  br i1 %ifcondition, label %then_br, label %else_br

then_br:                                          ; preds = %entry
  br label %ifcontext

else_br:                                          ; preds = %entry
  %ast_14 = load i64, ptr %ast_11, align 4
  %subtmp = sub i64 %ast_14, 1
  %anf_4 = alloca i64, align 8
  store i64 %subtmp, ptr %anf_4, align 4
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_0 to i64), i64 1)
  %anf_45 = load i64, ptr %anf_4, align 4
  %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 %anf_45)
  %anf_5 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_5, align 4
  %ast_16 = load i64, ptr %ast_11, align 4
  %anf_57 = load i64, ptr %anf_5, align 4
  %multmp = mul i64 %ast_16, %anf_57
  %anf_6 = alloca i64, align 8
  store i64 %multmp, ptr %anf_6, align 4
  %anf_68 = load i64, ptr %anf_6, align 4
  br label %ifcontext

ifcontext:                                        ; preds = %else_br, %then_br
  %ifphi = phi i64 [ 1, %then_br ], [ %anf_68, %else_br ]
  %anf_7 = alloca i64, align 8
  store i64 %ifphi, ptr %anf_7, align 4
  %anf_79 = load i64, ptr %anf_7, align 4
  ret i64 %anf_79
}

define i64 @ast_2() {
entry:
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_0 to i64), i64 1)
  ret i64 %PAppliClosure
}

define i64 @main() {
entry:
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @ast_2 to i64), i64 0)
  %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 5)
  %anf_1 = alloca i64, align 8
  store i64 %PAppliApplication, ptr %anf_1, align 4
  %PAppliClosure1 = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
  %anf_12 = load i64, ptr %anf_1, align 4
  %PAppliApplication3 = call i64 @applyPAppli(i64 %PAppliClosure1, i64 %anf_12)
  %anf_2 = alloca i64, align 8
  store i64 %PAppliApplication3, ptr %anf_2, align 4
  %anf_24 = load i64, ptr %anf_2, align 4
  ret i64 0
}
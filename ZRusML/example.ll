declare i64 @print_bool(i64)

declare i64 @print_int(i64)

declare i64 @applyPAppli(i64, i64)

declare i64 @addNewPAppliClosure(i64, i64)

define i64 @main() {
entry:
  %anf_1 = alloca i64, align 8
  store i64 9, i64* %anf_1, align 4
  %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (i64 (i64)* @print_int to i64), i64 1)
  %anf_11 = load i64, i64* %anf_1, align 4
  %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 %anf_11)
  %anf_2 = alloca i64, align 8
  store i64 %PAppliApplication, i64* %anf_2, align 4
  %anf_22 = load i64, i64* %anf_2, align 4
  ret i64 0
}


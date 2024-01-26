  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x);;
  > let main = print_int (fibo 11);;
  > EOF
  89

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > in 
  > fack n (fun x -> x);;
  > let main = print_int (fac 6);;
  > EOF
  720

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let main = print_int ( (5 + 4) - 2 );;
  > EOF
  7

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let x = (5 + (4 - 3)) - 2;;
  > let main = print_int x;;
  > EOF
  4

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let main = print_int ((5 + 4) + (3 + 2));;
  > EOF
  14

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3;;
  > let main = print_int (s1 10);;
  > EOF
  20

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let plus a =
  > let sum b = a + b in
  > sum 5;;
  > let main = print_int (plus 5);;
  > EOF
  10

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1);;
  > let main = print_int (factorial 5);;
  > EOF
  120

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec foo a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1 = a1;;
  > let main = foo 1;;
  > EOF

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec foo    a b c d e f g h i j k l m n  o p q  = a;;
  > let main = print_int (foo 1 2 3 4 5 6 7 8 9 0 1 2 3 4  5 6 7);;
  > EOF
  1

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec foo    _ _ _ _ e f _  = e;;
  > let k = foo 1 2 3 4 5 6 7;;
  > let main = print_int k;;
  > EOF
  5

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let x = 15;;
  > let main = print_int (-x);;
  -15

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let x = true;;
  > let main = print_bool (not x);;
  false

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let x = (5 + 4) + (-4);;
  > let main = print_int (-x);;
  -5

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec fac n = if n <= 1 then 1 else n * (fac (n - 1));;
  > let main = print_int (fac 10);;
  3628800

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let factorial n =
  >   let rec fac acc n = if n <= 1 then acc else fac (acc * n) (n - 1) in
  >   fac 1 n
  > ;;
  > let main = print_int (factorial 10);;
  3628800

  $ ./llvm_test.exe <<- EOF 
  > let x = 15;;
  > let y = x = true;;
  Error in №2 declaration: 
  Elaboration failed: Rules disagree on type: Cannot merge bool and int

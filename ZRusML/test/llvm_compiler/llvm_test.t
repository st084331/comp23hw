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

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);;
  > let main = print_int (fib 10);;
  89

  $ ./llvm_test.exe <<- EOF 
  > let x = 15;;
  > let y = x = true;;
  Error in №2 declaration: 
  Elaboration failed: Rules disagree on type: Cannot merge bool and int

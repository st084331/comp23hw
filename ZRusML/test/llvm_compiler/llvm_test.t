  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let f_wrapper f n = if n <= 1 then 0 else ((fun y -> y 15 + f (n - 1)) (fun t -> 15 * t + f (n - 1)));;
  > let rec f n = f_wrapper f n;;
  > let main = print_int (f 10);;
  114975

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let fibo n =
  >   let rec fibo_cps n acc =
  >     if n < 3 then acc 1 else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  >   in
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
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1);;
  > let main = print_int (factorial 5);;
  > EOF
  120

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
  > let rec print_helper cps n = if n = 0 then cps 1 else print_helper (fun x -> let _ = print_int (n - 2 * (n / 2)) in cps 1) (n / 2);;
  > let print_binary = print_helper (fun x -> x);;
  > let main = print_binary 23;;
  10111

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let rec bin_pow base exp =
  >   if exp = 0 then 
  >      1 
  >   else if (exp = 2 * (exp / 2)) then
  >     let tmp = bin_pow base (exp / 2)
  >     in tmp * tmp
  >   else
  >     base * (bin_pow base (exp - 1));;
  > 
  > let test_print base exp =
  >   let _ = print_int (bin_pow base exp) in print_endline
  > ;;
  > 
  > let main = 
  >   let test1 = test_print 32 4 in
  >   let test2 = test_print 7 4 in
  >   let test3 = test_print 2 10 in
  > 0;;
  
  1048576
  2401
  1024

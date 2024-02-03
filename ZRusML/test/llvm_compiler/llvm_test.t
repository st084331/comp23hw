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
  >   let _ = print_int (bin_pow base exp) in
  >   print_char 32
  > ;;
  > 
  > let main = 
  >   let test1 = test_print 32 4 in
  >   let test2 = test_print 7 4 in
  >   let test3 = test_print 2 10 in
  > 0;;
  1048576 2401 1024 

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so
  > let x = -3 + 4 + (5 + 9 - 1) / 3 * 4 - 8;;
  > let main = print_int x;;
  9

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let main =
  >   let test1 = print_bool (true && (5 > 3)) in
  >   let test2 = print_bool (false || 7 = 7) in
  >   let test3 = print_bool (17 * 4 > 3 || 2 < 4 / 4) in
  >   let test4 = print_bool (5 < 3 || 7 * 7 = 50 || 15 <= 14 || 9 * 3 <> 27) in
  > print_endline;;
  truetruetruefalse

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let f a b c d e f g = e;;
  > let main =
  >   let test1 = print_int (f 1 2 3 4 5 6 7) in
  >   let test2 = print_bool (f 1 2 3 4 true 6 7) in
  >   let test3 = print_int (f false true false 17 42 true 1) in
  > print_endline;;
  5true42

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let f _ _ x y z _ _ = x * y;;
  > let main = 
  >   let m t = 2 * t in
  >   let test10 = print_int (f true 4 15 3 true 6 false) 
  > in test10;;
  45

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let f _ _ x y z _ _ = x * y;;
  > let main = 
  >   let m t = 2 * t in
  >   let test10 = print_int (m (f true 4 15 3 true 6 false)) 
  > in test10;;
  90

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let f _ _ x y z _ _ = x * y;;
  > let main = 
  >   let m t = 2 * t in
  >   let test10 = print_int (m (f true 4 15 3 true 6 false)) 
  > in test10;;
  90

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let f x x x = x;;
  > let main = print_int (f 42 true 3);;
  3

  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/runtime.so 
  > let sum a b = a + b;;
  > let plus42 = sum 42;;
  > let main = print_int (plus42 15);;
  57


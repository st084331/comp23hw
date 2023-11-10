  $ ./anfTests.exe <<-EOF
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > in 
  > fack n (fun x -> x)
  > EOF
  let id_1 k n m = let 0_b_op = m * n in
   let 1_app = k 0_b_op in
   1_app
  let rec id_0 n k = let 0_b_op = n <= 1 in
   let 1_app = k 1 in
   let 2_b_op = n - 1 in
   let 3_app = id_0 2_b_op in
   let 4_app = id_1 k in
   let 5_app = 4_app n in
   let 6_app = 3_app 5_app in
   if 0_b_op then 1_app else 6_app
  let id_2 x = x
  let fac n = let 0_app = id_0 n in
   let 1_app = 0_app id_2 in
   1_app
  $ ./anfTests.exe <<-EOF
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > EOF
  let id_2 acc x y = let 0_b_op = x + y in
   let 1_app = acc 0_b_op in
   1_app
  let id_1 acc fibo_cps n x = let 0_b_op = n - 2 in
   let 1_app = fibo_cps 0_b_op in
   let 2_app = id_2 acc in
   let 3_app = 2_app x in
   let 4_app = 1_app 3_app in
   4_app
  let rec id_0 n acc = let 0_b_op = n < 3 in
   let 1_app = acc 1 in
   let 2_b_op = n - 1 in
   let 3_app = id_0 2_b_op in
   let 4_app = id_1 acc in
   let 5_app = 4_app id_0 in
   let 6_app = 5_app n in
   let 7_app = 3_app 6_app in
   if 0_b_op then 1_app else 7_app
  let id_3 x = x
  let fibo n = let 0_app = id_0 n in
   let 1_app = 0_app id_3 in
   1_app
  $ ./anfTests.exe <<-EOF
  > let x = (5 + 4) - 2
  > EOF
  let x  = let 0_b_op = 5 + 4 in
   let 1_b_op = 0_b_op - 2 in
   1_b_op
  $ ./anfTests.exe <<-EOF
  > let x = (5 + (4 - 3)) - 2
  > EOF
  let x  = let 0_b_op = 4 - 3 in
   let 1_b_op = 5 + 0_b_op in
   let 2_b_op = 1_b_op - 2 in
   2_b_op
  $ ./anfTests.exe <<-EOF
  > let x = (5 + 4) + (3 + 2)
  > EOF
  let x  = let 0_b_op = 5 + 4 in
   let 1_b_op = 3 + 2 in
   let 2_b_op = 0_b_op + 1_b_op in
   2_b_op
  $ ./anfTests.exe <<-EOF
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3
  > EOF
  let s1 x = let 0_b_op = x + 5 in
   let s2 = 0_b_op in
   let 1_b_op = s2 + 5 in
   let s3 = 1_b_op in
   s3
  $ ./anfTests.exe <<-EOF
  > let plus a =
  > let sum b = a + b in
  > sum 5
  > EOF
  let id_0 a b = let 0_b_op = a + b in
   0_b_op
  let plus a = let 0_app = id_0 a in
   let 1_app = 0_app 5 in
   1_app
  $ ./anfTests.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 5
  > EOF
  let rec factorial n = let 0_b_op = n <= 1 in
   let 1_b_op = n - 1 in
   let 2_app = factorial 1_b_op in
   let 3_b_op = n * 2_app in
   if 0_b_op then 1 else 3_b_op
  let main  = let 0_app = factorial 5 in
   0_app
  $ ./anfTests.exe <<-EOF
  > let a c d =
  > let m = c + d in
  > let k l = l + m in
  > k (5 + m)
  > EOF
  let id_0 m l = let 0_b_op = l + m in
   0_b_op
  let a c d = let 0_b_op = c + d in
   let m = 0_b_op in
   let 1_app = id_0 m in
   let 2_b_op = 5 + m in
   let 3_app = 1_app 2_b_op in
   3_app

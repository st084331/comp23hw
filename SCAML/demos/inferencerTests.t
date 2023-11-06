  $ ./inferencerTests.exe <<-EOF
  > let rec factorial n = if n <= 1  then 1 else factorial (n - 1) * n
  > let x5 = factorial 5 
  > EOF
  factorial : int -> int
  x5 : int
  $ ./inferencerTests.exe <<-EOF
  > let rec fibonacci n = if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)
  > let x5 = fibonacci 5 
  > EOF
  fibonacci : int -> int
  x5 : int
  $ ./inferencerTests.exe <<-EOF
  > let fack1 k n m = k (n * m)
  > let rec fack n k = if n <= 1 then k 1 else fack (n-1) (fack1 k n)
  > let id x = x
  > let fac n = fack n id
  > EOF
  fack1 : (int -> 'd) -> int -> int -> 'd
  fack : int -> (int -> 'm) -> 'm
  id : 'n -> 'n
  fac : int -> int
  $ ./inferencerTests.exe <<-EOF
  > let id x = x
  > let sum x y = x + y
  > let cont_maker helper_ctx n x = helper_ctx (n - 2) (sum x)
  > let rec helper n cont = if n < 3 then cont 1 else cont (helper (n - 1) (cont_maker helper n))
  > let fib_cps n = helper n id
  > EOF
  id : 'a -> 'a
  sum : int -> int -> int
  cont_maker : (int -> (int -> int) -> 'i) -> int -> int -> 'i
  helper : int -> (int -> int) -> int
  fib_cps : int -> int

% arithmetic operations test
  $ ./inferencer_test.exe << EOF
  > let x = 3 * 4 / 2 + 7;;
  > let y = 15 + 7 / 2;;
  > let result = x + y * x;;
  val x : int
  val y : int
  val result : int
  _______

% let rec test
  $ ./inferencer_test.exe << EOF
  > let rec timer = fun y -> if y = 0 then 0 else timer (y - 1);;
  > let tmp = timer 17;;
  val timer : int -> int
  val tmp : int
  _______


% binary operator inference test
  $ ./inferencer_test.exe << EOF
  > let e = fun x -> (if x >= 2 then x = 15 else false);;
  val e : int -> bool
  _______

% typs test
  $ ./inferencer_test.exe << EOF
  > let int = 3;;
  > let bool = true;;
  val int : int
  val bool : bool
  _______

% factorial test
  $ ./inferencer_test.exe << EOF
  > let rec fact n = if n = 0 then 1 else n * (fact (n - 1));;
  val fact : int -> int
  _______

% factorial acc test
  $ ./inferencer_test.exe << EOF
  > let rec acc_fact acc n = if n = 0 then acc else acc_fact (acc * n) (n - 1);;
  > let test = acc_fact 1 3 = 6;;
  val acc_fact : int -> int -> int
  val test : bool
  _______

% unbound id test
  $ ./inferencer_test.exe << EOF
  > let rec x y = x (y - 1);;
  > let z = y;;
  Error in №2 declaration: 
  Elaboration failed: Unbound value identifier y
  _______

% unification failed test
  $ ./inferencer_test.exe << EOF
  > let y = 4;;
  > let x = true;;
  > let z = x = y;;
  Error in №3 declaration: 
  Elaboration failed: Rules disagree on type: Cannot merge int and bool
  _______

% many arguments test
  $ ./inferencer_test.exe << EOF
  > let rec foo _ _ _ _ e f _ = e;;
  > let k = foo 1 2 3 4 5 6 7;;
  val foo : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'e
  val k : int
  _______

% many arguments test
  $ ./inferencer_test.exe << EOF
  >     let f _ _ x y z _ _ = x * y;;
  > let main = 
  >   let m t = 2 * t in
  >   let tmp = m (f true 4 15 3 true 6 false) in
  > tmp;;
  val f : 'a -> 'b -> int -> int -> 'e -> 'f -> 'g -> int
  val main : int
  _______

% kakadu jaml test
  $ ./inferencer_test.exe << EOF
  >  let rec fix = fun f -> (fun x -> f (fix f) x);;
  >  let fac = fun self -> (fun n -> (fun k -> if n<2 then k 1 else self (n-1) (fun a -> k (n*a))));;
  >  let fac = (fun a -> fix fac a);;
  >  let z = fac 5 (fun a -> a);;
  val fix : (('c -> 'f) -> ('c -> 'f)) -> ('c -> 'f)
  val fac : (int -> (int -> 'j) -> 'j) -> (int -> (int -> 'j) -> 'j)
  val fac : int -> (int -> 'l) -> 'l
  val z : int
  _______

  $ ./inferencer_test.exe << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x);;
  > let fib_cps self n cont = if n <= 1 then cont n else self (n-1) (fun val1 -> self (n-2) (fun val2 -> cont (val1 + val2)));;
  > let fib_cps = fix fib_cps;;
  > let fib n = fib_cps n (fun x -> x);;
  val fix : (('c -> 'f) -> ('c -> 'f)) -> ('c -> 'f)
  val fib_cps : (int -> (int -> 'j) -> 'j) -> (int -> (int -> 'j) -> 'j)
  val fib_cps : int -> (int -> 'k) -> 'k
  val fib : int -> int
  _______

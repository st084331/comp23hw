  $ ./demoANF.exe <<-EOF
  > let x = let g = 123 in g in x
  fn main =
    x.g = 123
    x = x.g
    x

  $ ./demoANF.exe <<-EOF
  > let f x = let g y = let h z w = z + w + x + y in h in g in f
  > EOF
  fn lambda.1 f.g.y, f.x, f.g.h.z, f.g.h.w =
    i.13 = (+ f.g.h.z)
    i.12 = (i.13 f.g.h.w)
    i.11 = (+ i.12)
    i.10 = (i.11 f.x)
    i.9 = (+ i.10)
    i.8 = (i.9 f.g.y)
    i.8
  fn f.g.h f.g.y, f.x, f.g.h.z =
    i.7 = (lambda.1 f.g.h.z)
    i.6 = (i.7 f.x)
    i.5 = (i.6 f.g.y)
    i.5
  fn f.g f.x, f.g.y =
    i.4 = (f.g.h f.x)
    i.3 = (i.4 f.g.y)
    i.3
  fn f f.x =
    i.2 = (f.g f.x)
    i.2
  fn main =
    f

  $ ./demoANF.exe <<-EOF
  > let f y = let g y = y + 1 in g in f
  > EOF
  fn f.g f.g.y =
    i.2 = (+ f.g.y)
    i.1 = (i.2 1)
    i.1
  fn f f.y =
    f.g
  fn main =
    f

  $ ./demoANF.exe <<-EOF
  > let rec fact n = if n <= 0 then 1 else n * fact (n - 1) in fact 4
  > EOF
  fn fact fact.n =
    i.3 = (<= fact.n)
    i.2 = (i.3 0)
    i.1 = if i.2 then
    1
    else  i.5 = (* fact.n)
    i.8 = (- fact.n)
    i.7 = (i.8 1)
    i.6 = (fact i.7)
    i.4 = (i.5 i.6)
    i.4
  
    i.1
  fn main =
    i.9 = (fact 4)
    i.9

  $ ./demoANF.exe <<-EOF
  > let fac n = let rec fack n k = if n <= 0 then k 1 else fack (n-1) (fun m -> k (m * n)) in fack n (fun x -> x) in fac
  > EOF
  fn lambda.2 fac.fack.n, fac.fack.k, fac.fack.m =
    i.22 = (* fac.fack.m)
    i.21 = (i.22 fac.fack.n)
    i.20 = (fac.fack.k i.21)
    i.20
  fn lambda.1 fac.fack.n, fac.fack.n, fac.fack, fac.fack.n, fac.fack.k =
    i.12 = (<= fac.fack.n)
    i.11 = (i.12 0)
    i.10 = if i.11 then
    i.13 = (fac.fack.k 1)
    i.13
    else  i.17 = (- fac.fack.n)
    i.16 = (i.17 1)
    i.15 = (fac.fack i.16)
    i.19 = (lambda.2 fac.fack.k)
    i.18 = (i.19 fac.fack.n)
    i.14 = (i.15 i.18)
    i.14
  
    i.10
  fn fac.fack fac.fack.n =
    i.9 = (lambda.1 fac.fack.n)
    i.8 = (i.9 fac.fack)
    i.7 = (i.8 fac.fack.n)
    i.6 = (i.7 fac.fack.n)
    i.6
  fn lambda.3 fac.x =
    fac.x
  fn fac fac.fack, fac.n =
    i.5 = (fac.fack fac.n)
    i.4 = (i.5 lambda.3)
    i.4
  fn main =
    i.23 = (fac fac.fack)
    i.23

  $ ./demoAnf.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > EOF
  fun cc_1 k n m = 
  let anf_1 = (m * n) in
  let anf_2 = k anf_1 in
  anf_2
  fun fack n k = 
  let anf_3 = (n <= 1) in
  let anf_4 = k 1 in
  let anf_5 = (n - 1) in
  let anf_6 = fack anf_5 in
  let anf_7 = cc_1 k in
  let anf_8 = anf_7 n in
  let anf_9 = anf_6 anf_8 in
  if anf_3 then anf_4 else anf_9
  fun ll_1 x = 
  x
  fun fac n = 
  let anf_10 = fack n in
  let anf_11 = anf_10 ll_1 in
  anf_11
  int -> int

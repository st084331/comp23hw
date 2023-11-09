let count = ref 0

let gen_var base_name () =
  incr count;
  Printf.sprintf "%s_%d" base_name !count
;;

let reset () = count := 0

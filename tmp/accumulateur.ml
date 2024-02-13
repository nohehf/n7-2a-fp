let fact n =
  (* factp is the value of fact(p) *)
  let rec fact' p factp = if p = n then factp else fact' (p + 1) (p * factp) in
  fact' 0 1
(*
    let fib n =
      let rec fib' p fibp =
        if n = p then fibp
        else fib' *)

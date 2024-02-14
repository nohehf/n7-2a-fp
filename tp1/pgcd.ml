(* Exercice à rendre *)
(*
   pgcd: int -> int -> int
   Calcule le pgcd de a et b
   Parametres: a et b, entiers naturels
   Precodition: a et b strictement positifs
*)
let rec pgcd a b =
  if a = b then a else if a > b then pgcd (a - b) b else pgcd a (b - a)

let%test _ = pgcd 1 1 = 1
let%test _ = pgcd 1 2 = 1
let%test _ = pgcd 2 1 = 1
let%test _ = pgcd 15 5 = 5
let%test _ = pgcd 5 15 = 5
let%test _ = pgcd 126 54 = 18
let%test _ = pgcd 54 126 = 18

(* primes *)
let%test _ = pgcd 3 7 = 1
let%test _ = pgcd 7 3 = 1

(* relatively primes *)
let%test _ = pgcd 20 9 = 1
let%test _ = pgcd 9 20 = 1

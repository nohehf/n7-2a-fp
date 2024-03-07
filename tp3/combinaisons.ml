(*** Combinaisons d'une liste ***)

(* CONTRAT 
Fonction qui calcule les combinaisons de k éléments parmi une liste l
Paramètre k : le nombre d'éléments à choisir
Paramètre l : la liste des éléments
Résultat : la liste des combinaisons de k éléments parmi l
*)
let rec combinaison k l = match k, l with 
  | 0, _ -> [[]]
  | _, [] -> []
  | _, t::q -> List.map (fun c -> t::c) (combinaison (k-1) q) @ (combinaison k q)

(* TESTS *)
let%test _ = combinaison 0 [] = [[]]
let%test _ = combinaison 3 [] = []
let%test _ = combinaison 0 [1;2;3;4] = [[]]
let%test _ = combinaison 1 [1; 2] = [[1];[2]]
let%test _ = combinaison 2 [1; 2] = [[1;2]]
let%test _ = combinaison 1 [1; 2; 3] = [[1]; [2]; [3]]
let%test _ = combinaison 2 [1; 2; 3] = [[1;2]; [1;3]; [2;3]]
let%test _ = combinaison 2 [2;3;4] = [[2;3]; [2;4]; [3;4]]
let%test _ = combinaison 3 [1;2;3;4] = [[1;2;3]; [1;2;4]; [1;3;4]; [2;3;4]]


open Encodage
open Chaines

(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)

(*************)
(* Exception *)
(*************)

exception NotALetter of char
exception CodeInvalide

(************)
(* Encodage *)
(************)

(* Cette fonction est présente dans la documentation OCaml mais pas dans toutes les versions d'OCaml *)
(* find_index : ('a -> bool) -> 'a list -> int option
   find_index f xs returns Some i, where i is the index of the first element of the list xs that satisfies f x, if there is such an element.
   It returns None if there is no such element.*)

let rec find_index p l =
  match l with
  | [] -> None
  | t :: q -> (
      if p t then Some 0
      else match find_index p q with None -> None | Some i -> Some (i + 1))

let%test _ = find_index (fun i -> i mod 2 = 0) [ 1; 3; 4; 5; 6; 7 ] = Some 2
let%test _ = find_index (fun i -> i mod 2 = 0) [ 1; 3; 5; 7 ] = None

let%test _ =
  find_index (fun c -> c > 'd') [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ] = Some 4

let%test _ =
  find_index (fun c -> c > 'r') [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ] = None

(* encoder_lettre : encode -> char -> (int * int) *)
(* Encode une lettre, c'est à dire indique la touche et le nombre de fois qu'il faut appuyer dessus pour saisir la lettre *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est la lettre à encoder *)
(* Renvoie un couple (touche, nombre de fois où il faut utiliser cette touche) *)
(* Exception si le caractère n'est pas une lettre minuscule *)
(* let encoder_lettre map chr = find_index (fun (digit, chars) ->
        (match find_index (fun c -> c = chr) chars with
            | None -> -1
            | Some(index) -> index
        )
        >= 0
    ) map *)

(*
     char -> bool
     Check si un char est une lettres minuscule (code compris entre 97 et 122 en ASCII)
*)
let rec is_valid_char c =
  let code = Char.code c in
  code >= 97 && code <= 122

let%test _ = is_valid_char 'a'
let%test _ = is_valid_char 'g'
let%test _ = is_valid_char 'z'
let%test _ = not (is_valid_char '1')
let%test _ = not (is_valid_char '.')

let rec encoder_lettre ((digit, chars) :: q) chr =
  if not (is_valid_char chr) then raise (NotALetter chr)
  else
    match find_index (fun c -> c = chr) chars with
    | None -> encoder_lettre q chr
    | Some index -> (digit, index + 1)

let%test _ = encoder_lettre t9_map 'a' = (2, 1)
let%test _ = encoder_lettre t9_map 'b' = (2, 2)
let%test _ = encoder_lettre t9_map 'c' = (2, 3)
let%test _ = encoder_lettre t9_map 'd' = (3, 1)
let%test _ = encoder_lettre t9_map 'e' = (3, 2)
let%test _ = encoder_lettre t9_map 'f' = (3, 3)
let%test _ = encoder_lettre t9_map 'g' = (4, 1)
let%test _ = encoder_lettre t9_map 'h' = (4, 2)
let%test _ = encoder_lettre t9_map 'i' = (4, 3)
let%test _ = encoder_lettre t9_map 'j' = (5, 1)
let%test _ = encoder_lettre t9_map 'k' = (5, 2)
let%test _ = encoder_lettre t9_map 'l' = (5, 3)
let%test _ = encoder_lettre t9_map 'm' = (6, 1)
let%test _ = encoder_lettre t9_map 'n' = (6, 2)
let%test _ = encoder_lettre t9_map 'o' = (6, 3)
let%test _ = encoder_lettre t9_map 'p' = (7, 1)
let%test _ = encoder_lettre t9_map 'q' = (7, 2)
let%test _ = encoder_lettre t9_map 'r' = (7, 3)
let%test _ = encoder_lettre t9_map 's' = (7, 4)
let%test _ = encoder_lettre t9_map 't' = (8, 1)
let%test _ = encoder_lettre t9_map 'u' = (8, 2)
let%test _ = encoder_lettre t9_map 'v' = (8, 3)
let%test _ = encoder_lettre t9_map 'w' = (9, 1)
let%test _ = encoder_lettre t9_map 'x' = (9, 2)
let%test _ = encoder_lettre t9_map 'y' = (9, 3)
let%test _ = encoder_lettre t9_map 'z' = (9, 4)

let%test _ =
  try
    let _ = encoder_lettre t9_map '&' in
    false
  with _ -> true

let%test _ = encoder_lettre stupide_map 'a' = (2, 1)
let%test _ = encoder_lettre stupide_map 'b' = (3, 1)
let%test _ = encoder_lettre stupide_map 'c' = (3, 2)
let%test _ = encoder_lettre stupide_map 'd' = (3, 3)
let%test _ = encoder_lettre stupide_map 'e' = (2, 2)
let%test _ = encoder_lettre stupide_map 'f' = (3, 4)
let%test _ = encoder_lettre stupide_map 'g' = (3, 5)
let%test _ = encoder_lettre stupide_map 'h' = (3, 6)
let%test _ = encoder_lettre stupide_map 'i' = (2, 3)
let%test _ = encoder_lettre stupide_map 'j' = (3, 7)
let%test _ = encoder_lettre stupide_map 'k' = (3, 8)
let%test _ = encoder_lettre stupide_map 'l' = (3, 9)
let%test _ = encoder_lettre stupide_map 'm' = (3, 10)
let%test _ = encoder_lettre stupide_map 'n' = (3, 11)
let%test _ = encoder_lettre stupide_map 'o' = (2, 4)
let%test _ = encoder_lettre stupide_map 'p' = (3, 12)
let%test _ = encoder_lettre stupide_map 'q' = (3, 13)
let%test _ = encoder_lettre stupide_map 'r' = (3, 14)
let%test _ = encoder_lettre stupide_map 's' = (3, 15)
let%test _ = encoder_lettre stupide_map 't' = (3, 16)
let%test _ = encoder_lettre stupide_map 'u' = (2, 5)
let%test _ = encoder_lettre stupide_map 'v' = (3, 17)
let%test _ = encoder_lettre stupide_map 'w' = (3, 18)
let%test _ = encoder_lettre stupide_map 'x' = (3, 19)
let%test _ = encoder_lettre stupide_map 'y' = (2, 6)
let%test _ = encoder_lettre stupide_map 'z' = (3, 20)

let%test _ =
  try
    let _ = encoder_lettre stupide_map '&' in
    false
  with _ -> true

(*
     creer_liste: int -> 'a -> 'a list
     Cree une liste contenant n * l'element e
     Parametres:
         - n: la taille de la liste
         - e: l'element pour remplir la liste
     Leve une exception Invalid_argument si n < 0

     Note: j'ai utilisé la librairie standard, mais l'implémetation est triviale
*)
let creer_liste n e = List.init n (fun _ -> e)

(* TESTS *)
let%test _ = creer_liste 0 'a' = []
let%test _ = creer_liste 0 [] = []
let%test _ = creer_liste 0 "hello" = []
let%test _ = creer_liste 0 1 = []
let%test _ = creer_liste 1 'a' = [ 'a' ]
let%test _ = creer_liste 1 [] = [ [] ]
let%test _ = creer_liste 1 "hello" = [ "hello" ]
let%test _ = creer_liste 1 1 = [ 1 ]
let%test _ = creer_liste 3 'a' = [ 'a'; 'a'; 'a' ]
let%test _ = creer_liste 3 [] = [ []; []; [] ]
let%test _ = creer_liste 3 "hello" = [ "hello"; "hello"; "hello" ]
let%test _ = creer_liste 3 1 = [ 1; 1; 1 ]

let%test _ =
  try
    let _ = creer_liste (-1) 'a' in
    false
  with _ -> true

(* encoder_mot : encodage -> string -> int list *)
(* Encode un mot, c'est à dire indique la suite de touche à appuyer pour saisir le mot *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est le mot à encoder *)
(* Renvoie la liste des touches à composer *)
(* Exception si le mot contient un caractère qui n'est pas une lettre minuscule *)

(* NOTE: J'ai ajouté une fonciton auxiliaire
   afin d'eviter un appel a décompose et recompose a chaque appel récursif
*)
let encoder_mot map word =
  let word_chars = decompose_chaine word in
  let rec encoder_list_chars chars =
    match chars with
    | [] -> []
    | chr :: q ->
        let digit, times = encoder_lettre map chr in
        creer_liste times digit @ [ 0 ] @ encoder_list_chars q
  in
  encoder_list_chars word_chars

let%test _ = encoder_mot t9_map "" = []
let%test _ = encoder_mot t9_map "a" = [ 2; 0 ]

let%test _ =
  encoder_mot t9_map "bonjour"
  = [ 2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0 ]

let%test _ =
  encoder_mot t9_map "ocaml"
  = [ 6; 6; 6; 0; 2; 2; 2; 0; 2; 0; 6; 0; 5; 5; 5; 0 ]

let%test _ =
  try
    let _ = encoder_mot t9_map "ab&c" in
    false
  with _ -> true

let%test _ = encoder_mot stupide_map "" = []
let%test _ = encoder_mot stupide_map "a" = [ 2; 0 ]

(* Note: désolé ocamlformat est pas très futé ici appremment *)
let%test _ =
  encoder_mot stupide_map "bonjour"
  = [
      3;
      0;
      2;
      2;
      2;
      2;
      0;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      0;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      0;
      2;
      2;
      2;
      2;
      0;
      2;
      2;
      2;
      2;
      2;
      0;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      0;
    ]

let%test _ =
  encoder_mot stupide_map "ocaml"
  = [
      2;
      2;
      2;
      2;
      0;
      3;
      3;
      0;
      2;
      0;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      0;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      3;
      0;
    ]

let%test _ =
  try
    let _ = encoder_mot stupide_map "ab&c" in
    false
  with _ -> true

(************)
(* Décodage *)
(************)

(* decoder_lettre -> encodage -> int * int -> char *)
(* Identifie la lettre saisie à partir d'une touche et du nombre de fois qu'elle a été appuyée *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est un couple (touche,  nombre de fois qu'elle a été appuyée) *)
(* Renvoie la lettre associée *)
(* Exception CodeInvalide si la combinaison ne correspond à aucune lettre *)

(* TODO code invalide *)
let decoder_lettre map (digit, times) =
  try List.nth (snd (List.find (fun (d, _) -> d = digit) map)) (times - 1)
  with _ -> raise CodeInvalide

let%test _ = decoder_lettre t9_map (2, 1) = 'a'
let%test _ = decoder_lettre t9_map (2, 2) = 'b'
let%test _ = decoder_lettre t9_map (2, 3) = 'c'
let%test _ = decoder_lettre t9_map (3, 1) = 'd'
let%test _ = decoder_lettre t9_map (3, 2) = 'e'
let%test _ = decoder_lettre t9_map (3, 3) = 'f'
let%test _ = decoder_lettre t9_map (4, 1) = 'g'
let%test _ = decoder_lettre t9_map (4, 2) = 'h'
let%test _ = decoder_lettre t9_map (4, 3) = 'i'
let%test _ = decoder_lettre t9_map (5, 1) = 'j'
let%test _ = decoder_lettre t9_map (5, 2) = 'k'
let%test _ = decoder_lettre t9_map (5, 3) = 'l'
let%test _ = decoder_lettre t9_map (6, 1) = 'm'
let%test _ = decoder_lettre t9_map (6, 2) = 'n'
let%test _ = decoder_lettre t9_map (6, 3) = 'o'
let%test _ = decoder_lettre t9_map (7, 1) = 'p'
let%test _ = decoder_lettre t9_map (7, 2) = 'q'
let%test _ = decoder_lettre t9_map (7, 3) = 'r'
let%test _ = decoder_lettre t9_map (7, 4) = 's'
let%test _ = decoder_lettre t9_map (8, 1) = 't'
let%test _ = decoder_lettre t9_map (8, 2) = 'u'
let%test _ = decoder_lettre t9_map (8, 3) = 'v'
let%test _ = decoder_lettre t9_map (9, 1) = 'w'
let%test _ = decoder_lettre t9_map (9, 2) = 'x'
let%test _ = decoder_lettre t9_map (9, 3) = 'y'
let%test _ = decoder_lettre t9_map (9, 4) = 'z'

let%test _ =
  try
    let _ = decoder_lettre t9_map (9, 6) in
    false
  with CodeInvalide -> true

let%test _ =
  try
    let _ = decoder_lettre t9_map (10, 1) in
    false
  with CodeInvalide -> true

let%test _ = decoder_lettre stupide_map (2, 1) = 'a'
let%test _ = decoder_lettre stupide_map (3, 1) = 'b'
let%test _ = decoder_lettre stupide_map (3, 2) = 'c'
let%test _ = decoder_lettre stupide_map (3, 3) = 'd'
let%test _ = decoder_lettre stupide_map (2, 2) = 'e'
let%test _ = decoder_lettre stupide_map (3, 4) = 'f'
let%test _ = decoder_lettre stupide_map (3, 5) = 'g'
let%test _ = decoder_lettre stupide_map (3, 6) = 'h'
let%test _ = decoder_lettre stupide_map (2, 3) = 'i'
let%test _ = decoder_lettre stupide_map (3, 7) = 'j'
let%test _ = decoder_lettre stupide_map (3, 8) = 'k'
let%test _ = decoder_lettre stupide_map (3, 9) = 'l'
let%test _ = decoder_lettre stupide_map (3, 10) = 'm'
let%test _ = decoder_lettre stupide_map (3, 11) = 'n'
let%test _ = decoder_lettre stupide_map (2, 4) = 'o'
let%test _ = decoder_lettre stupide_map (3, 12) = 'p'
let%test _ = decoder_lettre stupide_map (3, 13) = 'q'
let%test _ = decoder_lettre stupide_map (3, 14) = 'r'
let%test _ = decoder_lettre stupide_map (3, 15) = 's'
let%test _ = decoder_lettre stupide_map (3, 16) = 't'
let%test _ = decoder_lettre stupide_map (2, 5) = 'u'
let%test _ = decoder_lettre stupide_map (3, 17) = 'v'
let%test _ = decoder_lettre stupide_map (3, 18) = 'w'
let%test _ = decoder_lettre stupide_map (3, 19) = 'x'
let%test _ = decoder_lettre stupide_map (2, 6) = 'y'
let%test _ = decoder_lettre stupide_map (3, 20) = 'z'

let%test _ =
  try
    let _ = decoder_lettre stupide_map (9, 1) in
    false
  with CodeInvalide -> true

let%test _ =
  try
    let _ = decoder_lettre stupide_map (4, 3) in
    false
  with CodeInvalide -> true

(* decoder_mot -> encodage -> int list -> string *)
(* Identifie le mot saisi à partir d'une suite de touches  *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est la liste des touches appuyées *)
(* Renvoie le mot saisi *)
(* Exception CodeInvalide si une partie de la combinaison ne correspond à aucune lettre *)

let decoder_mot _ = assert false

(*
let%test _ = decoder_mot t9_map [] = ""
let%test _ = decoder_mot t9_map [2;0] = "a"
let%test _ = decoder_mot t9_map [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0] = "bonjour"
let%test _ = decoder_mot t9_map [6; 6; 6; 0; 2; 2; 2; 0; 2; 0; 6; 0; 5; 5; 5; 0] = "ocaml"
let%test _ = try let _ = decoder_mot t9_map [2; 2; 2; 2; 0] in false with CodeInvalide -> true

let%test _ = decoder_mot stupide_map [] = ""
let%test _ = decoder_mot stupide_map [2;0] = "a"
let%test _ = decoder_mot stupide_map  
    [3; 0; 2; 2; 2; 2; 0; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 0; 3; 3; 3; 3; 3; 3; 3;
     0; 2; 2; 2; 2; 0; 2; 2; 2; 2; 2; 0; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
     0] = "bonjour"
let%test _ = decoder_mot stupide_map
    [2; 2; 2; 2; 0; 3; 3; 0; 2; 0; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 0; 3; 3; 3; 3; 3;
     3; 3; 3; 3; 0] = "ocaml"
let%test _ = try let _ = decoder_mot stupide_map [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0] in false with _ -> true  
*)

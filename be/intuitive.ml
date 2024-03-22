open Encodage
open Chaines

(* Saisie des mots en mode T9 *)

(************)
(* Encodage *)
(************)

let rec find_index p l =
  match l with
  | [] -> None
  | t :: q -> (
      if p t then Some 0
      else match find_index p q with None -> None | Some i -> Some (i + 1))

(* encoder_lettre : encode -> char -> int *)
(* Encode une lettre, c'est à dire indique la touche qu'il faut appuyer pour saisir la lettre *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est la lettre à encoder *)
(* Renvoie la touche à utiliser si c'est une lettre minuscule , 0 pour les autres caractères *)
let encoder_lettre map chr =
  try
    fst
      (List.find
         (fun (_, chars) ->
           match find_index (fun c -> c = chr) chars with
           | None -> false
           | Some _ -> true)
         map)
  with _ -> 0

let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'b' = 2
let%test _ = encoder_lettre t9_map 'c' = 2
let%test _ = encoder_lettre t9_map 'd' = 3
let%test _ = encoder_lettre t9_map 'e' = 3
let%test _ = encoder_lettre t9_map 'f' = 3
let%test _ = encoder_lettre t9_map 'g' = 4
let%test _ = encoder_lettre t9_map 'h' = 4
let%test _ = encoder_lettre t9_map 'i' = 4
let%test _ = encoder_lettre t9_map 'j' = 5
let%test _ = encoder_lettre t9_map 'k' = 5
let%test _ = encoder_lettre t9_map 'l' = 5
let%test _ = encoder_lettre t9_map 'm' = 6
let%test _ = encoder_lettre t9_map 'n' = 6
let%test _ = encoder_lettre t9_map 'o' = 6
let%test _ = encoder_lettre t9_map 'p' = 7
let%test _ = encoder_lettre t9_map 'q' = 7
let%test _ = encoder_lettre t9_map 'r' = 7
let%test _ = encoder_lettre t9_map 's' = 7
let%test _ = encoder_lettre t9_map 't' = 8
let%test _ = encoder_lettre t9_map 'u' = 8
let%test _ = encoder_lettre t9_map 'v' = 8
let%test _ = encoder_lettre t9_map 'w' = 9
let%test _ = encoder_lettre t9_map 'x' = 9
let%test _ = encoder_lettre t9_map 'y' = 9
let%test _ = encoder_lettre t9_map 'z' = 9
let%test _ = encoder_lettre t9_map '&' = 0
let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'b' = 3
let%test _ = encoder_lettre stupide_map 'c' = 3
let%test _ = encoder_lettre stupide_map 'd' = 3
let%test _ = encoder_lettre stupide_map 'e' = 2
let%test _ = encoder_lettre stupide_map 'f' = 3
let%test _ = encoder_lettre stupide_map 'g' = 3
let%test _ = encoder_lettre stupide_map 'h' = 3
let%test _ = encoder_lettre stupide_map 'i' = 2
let%test _ = encoder_lettre stupide_map 'j' = 3
let%test _ = encoder_lettre stupide_map 'k' = 3
let%test _ = encoder_lettre stupide_map 'l' = 3
let%test _ = encoder_lettre stupide_map 'm' = 3
let%test _ = encoder_lettre stupide_map 'n' = 3
let%test _ = encoder_lettre stupide_map 'o' = 2
let%test _ = encoder_lettre stupide_map 'p' = 3
let%test _ = encoder_lettre stupide_map 'q' = 3
let%test _ = encoder_lettre stupide_map 'r' = 3
let%test _ = encoder_lettre stupide_map 's' = 3
let%test _ = encoder_lettre stupide_map 't' = 3
let%test _ = encoder_lettre stupide_map 'u' = 2
let%test _ = encoder_lettre stupide_map 'v' = 3
let%test _ = encoder_lettre stupide_map 'w' = 3
let%test _ = encoder_lettre stupide_map 'x' = 3
let%test _ = encoder_lettre stupide_map 'y' = 2
let%test _ = encoder_lettre stupide_map 'z' = 3
let%test _ = encoder_lettre stupide_map '&' = 0

(* encoder_mot : encodage -> string -> int list *)
(* Encode un mot, c'est à dire indique la suite de touche à appuyer pour saisir le mot *)
(* Le premier paramètre est la liste d'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est le mot à encoder *)
(* Renvoie la liste des touches à composer *)

let encoder_mot map word =
  let word_chars = decompose_chaine word in
  let rec encoder_list_chars chars =
    match chars with
    | [] -> []
    | chr :: q ->
        let digit = encoder_lettre map chr in
        digit :: encoder_list_chars q
  in
  encoder_list_chars word_chars

let%test _ = encoder_mot t9_map "" = []
let%test _ = encoder_mot t9_map "a" = [ 2 ]
let%test _ = encoder_mot t9_map "bonjour" = [ 2; 6; 6; 5; 6; 8; 7 ]
let%test _ = encoder_mot t9_map "bonjour!" = [ 2; 6; 6; 5; 6; 8; 7; 0 ]
let%test _ = encoder_mot t9_map "ocaml" = [ 6; 2; 2; 6; 5 ]
let%test _ = encoder_mot stupide_map "" = []
let%test _ = encoder_mot stupide_map "a" = [ 2 ]
let%test _ = encoder_mot stupide_map "bonjour" = [ 3; 2; 3; 3; 2; 2; 3 ]
let%test _ = encoder_mot stupide_map "bonjour!" = [ 3; 2; 3; 3; 2; 2; 3; 0 ]
let%test _ = encoder_mot stupide_map "ocaml" = [ 2; 3; 2; 3; 3 ]

(***************************************************************************)
(* Décodage                                                                *)
(* Nécessite un dictionnaire car un même code est associé à plusieurs mots *)
(***************************************************************************)

(* Arbre stockant les mots d'un dictionnaire en les associant à une suite de touche *)
(* Les branches de l'arbre ne sont pas forcément triées *)
type dico = Noeud of (string list * (int * dico) list)

(* empty : dico *)
(* Dictionnaire vide *)
let empty = Noeud ([], [])

(* TP4 *)
let rec recherche c lb =
  match lb with
  | [] -> None
  | (tc, ta) :: qlb -> if c = tc then Some ta else recherche c qlb

let rec maj c nouvelle_b lb =
  match lb with
  | [] -> [ (c, nouvelle_b) ]
  | (tc, ta) :: qlb ->
      if c < tc then (c, nouvelle_b) :: lb
      else if c = tc then (c, nouvelle_b) :: qlb
      else (tc, ta) :: maj c nouvelle_b qlb

let rec ajouter_digits (Noeud (words, childs)) word digits =
  match digits with
  | [] -> Noeud (word :: words, childs)
  | d :: q ->
      let dico_d =
        let l = recherche d childs in
        match l with None -> Noeud ([], []) | Some dd -> dd
      in
      Noeud (words, maj d (ajouter_digits dico_d word q) childs)

(* ajouter : encodage -> dico -> string -> dico *)
(* Ajoute un mot à un dictionnaire en respectant un encodage *)
(* Le premier paramètre est l'encodage (ie quelle touche correspond à quelles lettres) *)
(* Le second paramètre est le dictionnaire dans lequel le mot doit être ajouté *)
(* Le troisième paramètre est le mot à ajouter *)
(* Renvoie le nouveau dictionnaire *)
let ajouter map dico word = ajouter_digits dico word (encoder_mot map word)

(* TESTS - ATTENTION les tests peuvent échouer car l'ordre des branches n'est pas fixé *)

let a1 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                ( 6,
                  Noeud
                    ( [],
                      [
                        ( 6,
                          Noeud
                            ( [],
                              [
                                ( 5,
                                  Noeud
                                    ( [],
                                      [
                                        ( 6,
                                          Noeud
                                            ( [],
                                              [
                                                ( 8,
                                                  Noeud
                                                    ( [],
                                                      [
                                                        ( 7,
                                                          Noeud
                                                            ([ "bonjour" ], [])
                                                        );
                                                      ] ) );
                                              ] ) );
                                      ] ) );
                              ] ) );
                      ] ) );
              ] ) );
      ] )

let%test _ = a1 = ajouter t9_map empty "bonjour"

let a2 =
  Noeud
    ( [],
      [
        ( 6,
          Noeud
            ( [],
              [
                ( 2,
                  Noeud
                    ( [],
                      [
                        ( 2,
                          Noeud
                            ( [],
                              [
                                (6, Noeud ([], [ (5, Noeud ([ "ocaml" ], [])) ]));
                              ] ) );
                      ] ) );
              ] ) );
      ] )

let%test _ = a2 = ajouter t9_map empty "ocaml"

let a3 =
  Noeud
    ( [],
      [
        (2, Noeud ([ "a" ], []));
        ( 6,
          Noeud
            ( [],
              [
                ( 2,
                  Noeud
                    ( [],
                      [
                        ( 2,
                          Noeud
                            ( [],
                              [
                                (6, Noeud ([], [ (5, Noeud ([ "ocaml" ], [])) ]));
                              ] ) );
                      ] ) );
              ] ) );
      ] )

let%test _ = a3 = ajouter t9_map a2 "a"
let a4 = Noeud ([], [ (2, Noeud ([], [ (8, Noeud ([ "au" ], [])) ])) ])
let%test _ = a4 = ajouter t9_map empty "au"

let a5 =
  Noeud
    ( [],
      [
        (2, Noeud ([], [ (6, Noeud ([ "an" ], [])); (8, Noeud ([ "au" ], [])) ]));
      ] )

let%test _ = a5 = ajouter t9_map a4 "an"

let a6 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                (6, Noeud ([ "an" ], [ (3, Noeud ([ "ane" ], [])) ]));
                (8, Noeud ([ "au" ], []));
              ] ) );
      ] )

let%test _ = a6 = ajouter t9_map a5 "ane"

let a7 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                (6, Noeud ([ "an" ], [ (3, Noeud ([ "ame"; "ane" ], [])) ]));
                (8, Noeud ([ "au" ], []));
              ] ) );
      ] )

let%test _ = a7 = ajouter t9_map a6 "ame"

let a8 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                ( 6,
                  Noeud ([ "an" ], [ (3, Noeud ([ "bof"; "ame"; "ane" ], [])) ])
                );
                (8, Noeud ([ "au" ], []));
              ] ) );
      ] )

let%test _ = a8 = ajouter t9_map a7 "bof"

let a9_1 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                ( 6,
                  Noeud ([ "an" ], [ (3, Noeud ([ "bof"; "ame"; "ane" ], [])) ])
                );
                (8, Noeud ([ "bu"; "au" ], []));
              ] ) );
      ] )

let a9_2 =
  Noeud
    ( [],
      [
        ( 2,
          Noeud
            ( [],
              [
                (8, Noeud ([ "bu"; "au" ], []));
                ( 6,
                  Noeud ([ "an" ], [ (3, Noeud ([ "bof"; "ame"; "ane" ], [])) ])
                );
              ] ) );
      ] )

let%test _ = a9_1 = ajouter t9_map a8 "bu" || a9_2 = ajouter t9_map a8 "bu"

(* decoder_mot -> dico -> int list -> string list *)
(* Identifie l'ensemble des mots potentiellement saisis à partir d'une suite de touches  *)
(* Le premier paramètre est le dictionnaire *)
(* Le second paramètre est la liste des touches appuyées *)
(* Renvoie le mot saisi *)

let rec get_dico (Noeud (words, childs)) digits =
  match digits with
  | [] -> Noeud (words, childs)
  | d :: q -> (
      match recherche d childs with
      | None -> failwith ""
      | Some dict -> get_dico dict q)

let rec decoder_mot (Noeud (words, childs)) digits =
  match digits with
  | [] -> words
  | d :: q -> (
      match recherche d childs with
      | None -> []
      | Some dict -> decoder_mot dict q)

let%test _ = decoder_mot a9_1 [ 2; 8 ] = [ "bu"; "au" ]
let%test _ = decoder_mot a9_1 [ 2; 6 ] = [ "an" ]
let%test _ = decoder_mot a9_1 [ 2; 6; 3 ] = [ "bof"; "ame"; "ane" ]
let%test _ = decoder_mot a9_1 [ 1; 4; 5 ] = []
let%test _ = decoder_mot a9_2 [ 2; 8 ] = [ "bu"; "au" ]
let%test _ = decoder_mot a9_2 [ 2; 6 ] = [ "an" ]
let%test _ = decoder_mot a9_2 [ 2; 6; 3 ] = [ "bof"; "ame"; "ane" ]
let%test _ = decoder_mot a9_2 [ 1; 4; 5 ] = []
let%test _ = decoder_mot empty [ 5; 4; 6 ] = []

(* Tests combinés de empty, ajoute et decoder_mot *)
(* Doivent passer, car ne dépendent pas de l'ordre dans les listes *)

(*
let creer_dico encodage file = 
  let channel = open_in file in
  try
    let rec read_lines acc =
      try
        let line = input_line channel in
        read_lines (ajouter encodage acc line)
      with End_of_file ->
        acc
    in
    let lines = read_lines empty in
    close_in channel;
    lines
  with e ->
    close_in channel;
    raise e

let dico_fr = creer_dico t9_map "dico_fr.txt"
let dico_fr_stupide = creer_dico stupide_map "dico_fr.txt"

let%test _ = decoder_mot dico_fr [2;6;6;5;6;8;7] = ["bonjour"]
let%test _ = decoder_mot dico_fr [8;3;6;3;7;3] = ["venere"; "vendre"; "tendre"]
let%test _ = decoder_mot dico_fr [2;6;3] = ["cod"; "cnd"; "bof"; "aof"; "ane"; "ame"]
let%test _ = decoder_mot dico_fr [8;3;7;3;6;3] = []
let%test _ = decoder_mot dico_fr [2;8;7;3;5;4;3] = ["aurelie"]

let%test _ = decoder_mot dico_fr_stupide [2;2;2] = ["yue"; "oui"; "oie"; "eau"; "eao"; "aie"]
let%test _ = decoder_mot dico_fr_stupide [3;3;3;3] = 
             ["tvhd"; "ssbs"; "sprl"; "slbm"; "sgml"; "sgbd"; "ppcm"; "pgcd"; "ntsc";
              "ndlr"; "msbs"; "mrbm"; "http"; "html"; "gprs"; "brrr"; "bcbg"]
let%test _ = decoder_mot dico_fr_stupide [3;2;3;2;3;2;3;2;3;2;3;2;3;2] = 
             ["toxicomanogene"; "seropositivite"; "recapitulative"; "monocotyledone";
              "hypovitaminose"; "hyperemotivite"; "heterometabole"; "desiderabilite"]
let%test _ = decoder_mot dico_fr_stupide [2;3;2;3;2;3;2;3;2;3;2;3;2;3;2] = ["aluminosilicate"; "adiposogenitale"]
let%test _ = decoder_mot dico_fr_stupide [4;3;2] = []
*)

(* max_mots_code_identique : dico -> int *)
(* Calcule le nombre maximum de mots associés à un même code dans un dictionnaire *)
let max_mots_code_identique _ = assert false

(*
let%test _ = max_mots_code_identique a9_1 = 3
let%test _ = max_mots_code_identique a9_2 = 3
let%test _ = max_mots_code_identique a8 = 3
let%test _ = max_mots_code_identique a7 = 2
let%test _ = max_mots_code_identique a6 = 1
let%test _ = max_mots_code_identique a5 = 1
let%test _ = max_mots_code_identique a4 = 1
let%test _ = max_mots_code_identique a3 = 1
let%test _ = max_mots_code_identique a2 = 1
let%test _ = max_mots_code_identique a1 = 1
let%test _ = max_mots_code_identique empty = 0
let%test _ = max_mots_code_identique dico_fr = 8
let%test _ = max_mots_code_identique dico_fr_stupide = 1155
*)

(* lister : dico -> string liste *)
(* Créer la liste de tous les mots d'un dictionnaire *)
let rec lister _ = assert false

(*
let%test _ = (List.sort (String.compare) (lister a9_1)) = ["ame";"an";"ane";"au";"bof";"bu"]
let%test _ = (List.sort (String.compare) (lister a9_2)) = ["ame";"an";"ane";"au";"bof";"bu"]
let%test _ = (List.sort (String.compare) (lister a6)) = ["an";"ane";"au"]
let%test _ = (List.sort (String.compare) (lister empty)) = []
*)

(* prefix : dico -> int list -> string list *)
(* Liste tous les mots dont le préfix est la liste de touche passée en paramètre *)
let rec prefix _ = assert false

(*
let%test _ = (List.sort (String.compare) (prefix a9_1 [2])) = ["ame";"an";"ane";"au";"bof";"bu"]
let%test _ = (List.sort (String.compare) (prefix a9_1 [2;6])) = ["ame";"an";"ane";"bof"]
let%test _ = (List.sort (String.compare) (prefix a9_1 [2;8])) = ["au";"bu"]
let%test _ = (List.sort (String.compare) (prefix a9_1 [3;8])) = []
let%test _ = (List.sort (String.compare) (prefix empty [3;8])) = []

let%test _ = (List.sort (String.compare) (prefix dico_fr [9;2;3])) = ["wading"; "zad"]
let%test _ = (List.sort (String.compare) (prefix dico_fr [2;6;6;5;6])) = 
             ["bonjour"; "bookmaker"; "conjoint"; "conjointe"; "conjointement";
              "conjoncteur"; "conjoncteurdisjoncteur"; "conjoncteursdisjoncteurs";
              "conjonctif"; "conjonction"; "conjonctival"; "conjonctivaux"; "conjonctive";
              "conjonctivite"; "conjoncture"; "conjoncturel"; "conjoncturelle";
              "conjoncturiste"]
let%test _ = (List.sort (String.compare) (prefix dico_fr [2;2;2;2;2])) =
             ["abaca"; "accablant"; "accable"; "accablement"; "accabler"; "babacool";
              "babbage"; "baccalaureat"; "baccara"; "baccarat"; "cacabant"; "cacabe";
              "cacaber"]
let%test _ = (List.sort (String.compare) (prefix dico_fr_stupide [2;2;2;2])) =
             ["aieul"; "aieule"; "aieuls"; "aieux"; "ayeaye"; "ayyubides"; "eoue";
              "ouaille"; "ouailles"; "ouais"; "ouaouaron"; "ouie"; "yaourt"; "yaourtiere";
              "yeye"; "youyou"; "yoyo"]
let%test _ = (List.sort (String.compare) (prefix dico_fr_stupide [3;3;3;3])) =
             ["bcbg"; "brrr"; "chthonien"; "chthonienne"; "dvdrom"; "gprs"; "html"; "http";
              "mrbm"; "msbs"; "ndlr"; "ntsc"; "pgcd"; "ppcm"; "pschent"; "schlague";
              "schlamms"; "schlass"; "schlinguant"; "schlingue"; "schlinguer"; "schlittage";
              "schlittant"; "schlitte"; "schlitter"; "schlitteur"; "schnaps"; "schnauzer";
              "schnock"; "schnoque"; "schnorchel"; "schnorkel"; "schnouf"; "schnouff";
              "schproum"; "sgbd"; "sgml"; "slbm"; "sprl"; "ssbs"; "tvhd"]
*)
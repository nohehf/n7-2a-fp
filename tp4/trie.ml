open Assoc
open Arbre
open Chaines

(* le type trie :
    triplet arbre,
            fonction de décomposition mot -> liste de caractères,
            fonction de recomposition liste de caractères -> mot *)
type ('a,'b) trie = Trie of ('b arbre) * ('a -> 'b list) * ('b list -> 'a)

(******************************************************************************)
(*   fonction de création d'un nouveau trie                                   *)
(*   signature  : nouveau :                                                   *)
(*          ('a -> 'b list) -> ('b list -> 'a) -> ('a, 'b) trie = <fun>       *)
(*   paramètres : - une fonction de décomposition                             *)
(*                     mot -> liste de caractères                             *)
(*                -  une fonction de recomposition                            *)
(*                     liste de caractères -> mot                             *)
(*   résultat     : un nouveau trie "vide"                                    *)
(******************************************************************************)
let nouveau fd fr = Trie(Noeud(false,[]), fd, fr)

(******************************************************************************)
(*   fonction d'ajout d'un élément dans un trie                               *)
(*   signature  : ajout : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun>        *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot ajouté                                  *)
(******************************************************************************)
let ajout mot (Trie(arbre, decompose, recompose)) =
  Trie (ajout_arbre (decompose mot) arbre,decompose,recompose)


(*  Pour les tests *)
let trie_sujet =
  List.fold_right ajout
    ["bas"; "bât"; "de"; "la"; "lai"; "laid"; "lait"; "lard"; "le"; "les"; "long"]
    (nouveau decompose_chaine recompose_chaine)

(******************************************************************************)
(*   fonction d'appartenance d'un élément à un trie                           *)
(*   signature  : appartient : 'a -> ('a, 'b) trie -> bool = <fun>            *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let appartient mot (Trie(arbre, fd, _)) = appartient_arbre (fd mot) arbre

let%test _ = appartient "bas"  trie_sujet
let%test _ = appartient "bât"  trie_sujet
let%test _ = appartient "de"  trie_sujet
let%test _ = appartient "la"  trie_sujet
let%test _ = appartient "lai"  trie_sujet
let%test _ = appartient "laid"  trie_sujet
let%test _ = appartient "lait"  trie_sujet
let%test _ = appartient "lard"  trie_sujet
let%test _ = appartient "le"  trie_sujet
let%test _ = appartient "les"  trie_sujet
let%test _ = appartient "long"  trie_sujet
let%test _ = not (appartient "toto" trie_sujet)
let%test _ = not (appartient "ba"  trie_sujet)
let%test _ = not (appartient "lon"  trie_sujet)
let%test _ = not (appartient ""  trie_sujet)

(******************************************************************************)
(*   fonction de retrait d'un élément d'un trie                               *)
(*   signature  : trie_retrait : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun> *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot retiré                                  *)
(******************************************************************************)
let rec retrait_arbre caracteres (Noeud(value, branches)) = match caracteres with 
  | [] -> Noeud(false, branches)
  | t::q -> match recherche t branches with 
    | None -> Noeud(value, branches)
    | Some(sub_arbre) -> Noeud(value, maj t (retrait_arbre q sub_arbre) branches)

let retrait mot (Trie(arbre, fd, fr)) = Trie(retrait_arbre (fd mot) arbre, fd, fr)

let trie_retrait =
  List.fold_right ajout
    ["bas"; "bat"]
    (nouveau decompose_chaine recompose_chaine)

let%test _ = not (appartient "bas" (retrait "bas" trie_retrait))
let%test _ = appartient "bat" trie_retrait
let%test _ = appartient "bat" (retrait "bas" trie_retrait)
let%test _ = appartient "bas" (retrait "" trie_retrait)



(******************************************************************************)
(*   fonction interne au Module qui génère la liste de tous les mots          *)
(*   d'un trie                                                                *)
(*   signature    : trie_dico : ('a, 'b) trie -> 'a list = <fun>              *)
(*   paramètre(s) : le trie                                                   *)
(*   résultat     : la liste des mots                                         *)
(******************************************************************************)
let trie_dico (Trie(arbre, _, fr)) = List.map fr (arbre_dico arbre)

(******************************************************************************)
(* procédure d'affichage d'un trie                                            *)
(*   signature  : affiche : ('a -> unit) -> ('a, 'b) trie -> unit = <fun>     *)
(*   paramètres : - une procédure d'affichage d'un mot                        *)
(*                - un trie                                                   *)
(*   résultat   : aucun                                                       *)
(******************************************************************************)
let affiche p trie = failwith "TO DO affiche"

(*
   Return the first element of a list
   Precondition: liste non vide
*)
let hd l = match l with [] -> failwith "liste vide" | a :: _ -> a

(*
   Return the tail of a list
   Precondition: liste non vide
*)
let tl l = match l with [] -> failwith "liste vide" | _ :: b -> b

(*
   Returns the size of a list
*)
let rec taille_lazy l = match l with [] -> 0 | _ :: b -> 1 + taille_lazy b

(*
   appends l2 at the end of l1
*)
let rec append l1 l2 = match l1 with [] -> l2 | a :: b -> a :: append b l2

(*
    Returns last element of l
*)
let rec last l =
  match l with [] -> failwith "liste vide" | a :: [] -> a | a :: b -> last b

(* ITERATEURS DE LISTE *)
(* Simple implementation of List.map *)
let rec map f l = match l with [] -> [] | a :: b -> f a :: map f b

(*
    Transforms an int list into a string list
*)
let string_of_int_list = List.map string_of_int

(*
    fold_right f l = [a, b, ...]  -> f(a, f(b, f(... f(e)))
*)
let rec fold_right f l e =
  match l with [] -> e | a :: b -> f a (fold_right f b e)

(*
   Implems with fold right
*)
let somme l = List.fold_right ( + ) l 0
let taille l = List.fold_right (fun _ x -> x + 1) l 0
let append l1 l2 = List.fold_right (fun t ql2 -> t :: ql2) l1 l2
(* let append_sugar l1 = List.fold_right ( :: ) l1 *)

(*
   @ means append
   The pattern is always fun (head queue -> ...)
*)
let rev_fr l = List.fold_right (fun t revq -> revq @ [ t ]) l []
let rev_fl l = List.fold_left (fun acc lst -> lst :: acc) [] l
let rev_fl_sugar = List.fold_left (fun acc lst -> lst :: acc) []

let plus1 = (1 +) 2
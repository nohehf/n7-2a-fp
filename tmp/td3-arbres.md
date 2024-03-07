# TD3 arbres lexicographiques

## Exercice 1

Single type:

```ocaml
type 'a arbre = Node of bool * ('a * 'a arbre) list
```

Double type:

```ocaml
type 'a branch = 'a * 'a arbre
type 'a arbre = Node of bool * 'a branch list
```

## Exercice 2

```ocaml
let rec appartient sequence Node(b, fils) = match sequence with
    | [] -> b (*si sequence vide*)
    | t1::q1 -> (*si sequence non vide: on veut le fils sous la branche = t si existe *)
            match fils with
            | [] -> false
            | (e, arbre)::q2 -> if t1 = e then appartient q1 arbre else appartient sequence Node(b, q2)
```

### Correction

```ocaml
let rec recherche e liste_branches = match liste_branches with
    | [] -> None
    | (branch_val, arbre)::q -> if e = branch_val then Some(arbre) else if e < branch_val then None else recherche e q

let rec appartient sequence Node(b, branches) = match sequence with
    | [] -> b
    | character::sub_sequence ->
            match recherche character branches with
            | None -> false
            | Some(sub_tree) -> appartient sub_sequence sub_tree
```

## Exercice 3

```ocaml
let rec maj branch_val sub_arbre branches = match branches with
    | [] -> [(branch_val, sub_arbre)]
    | (v, arbres)::q -> if v = branch_val then (v, arbres)::q else if v < branch_val then (v, arbres)::(maj branch_val sub_arbre q) else (v, arbres)::(branch_val,sub_arbre)::q

let rec ajout sequence Node(b, branches) = match sequence with
    | [] -> Node(true, branches)
    | character::sub_sequence -> match recherche character branches with
        | None -> maj character (ajout sub_sequence Node(b, [])) branches
        | Some(sub_arbre) -> Node(b, maj character (ajout sub_sequence sub_arbre) branches)
```

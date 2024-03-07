type 'a arbre_naire = Node of 'a * 'a arbre_naire list

let const e childs = Node (e, childs)
let racine (Node (r, _)) = r
let fils (Node (_, f)) = f
let rec map f (Node (e, fils)) = Node (f e, List.map (map f) fils)
let rec fold f (Node (e, fils)) = f e (List.map (fold f) fils)

let rec cardinal =
  fold (fun _ childs -> match childs with [] -> 0 | _ -> List.length childs)

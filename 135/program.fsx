type Tree = 
    | Empty 
    | Node of int * Tree * Tree

let rec minPath (t: Tree) acc: int =
    match t with
    | Node (v, l, r) -> min (minPath l (acc + v)) (minPath r (acc + v))
    | Empty -> acc


let rec insert (t: Tree) target =
    match t with
    | Empty -> Node (target, Empty, Empty)
    | Node (value, left, right) ->
        if target < value then  
            Node (value, (insert left target), right)
        else if target > value then
            Node (value, left, (insert right target))
        else
            t


let fromSequence sequence: Tree =
    Seq.fold insert Empty sequence

// Create an example tree
let leaf v = Node (v, Empty, Empty)
let node v l r = Node (v, l, r);;
let tree = node 10 (node 5 Empty (leaf 2)) (node 5 Empty (node 1 Empty (leaf -1)));;


minPath tree 0
    |> printfn "%A"

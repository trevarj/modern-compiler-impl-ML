type key = string
type tree = Leaf | Tree of tree * key * tree

let empty = Leaf

(* Insertion into a persistent binary tree *)
let rec (insert : key * tree -> tree) = function
  | key, Leaf -> Tree (Leaf, key, Leaf)
  | key, Tree (l, k, r) when key < k -> Tree (insert (key, l), k, r)
  | key, Tree (l, k, r) when key > k -> Tree (l, k, insert (key, r))
  | same -> snd same

let rec member key = function
  | Leaf -> false
  | Tree (l, k, r) -> key == k || member key l || member key r

(* Binary tree that can be used as a symbol table for the single-line interpreter *)
module BinaryTree = struct
  type key = string
  type 'a t = Leaf | Tree of 'a t * (key * 'a) * 'a t

  (* Insert a key and value into a tree *)
  let rec insert : key * 'a * 'a t -> 'a t = function
    | k, v, Leaf -> Tree (Leaf, (k, v), Leaf)
    | k, v, Tree (l, pair, r) when k < fst pair ->
        Tree (insert (k, v, l), pair, r)
    | k, v, Tree (l, pair, r) when k > fst pair ->
        Tree (l, pair, insert (k, v, r))
    | k, v, Tree (l, _, r) -> Tree (l, (k, v), r)

  (* Returns whether a key is present in the tree*)
  let rec member key = function
    | Leaf -> false
    | Tree (l, pair, r) -> key = fst pair || member key l || member key r

  (* Looks up a value by key in the tree *)
  let rec lookup key = function
    | Tree (_, pair, _) when key = fst pair -> Some (snd pair)
    | Tree (l, _, r) -> (
        match lookup key l with None -> lookup key r | some -> some)
    | _ -> None
end

(* Tests *)
let%test_module _ =
  (module struct
    open BinaryTree

    let empty = Leaf
    let single = Tree (Leaf, ("y", 2), Leaf)
    let double = Tree (Tree (Leaf, ("x", 1), Leaf), ("y", 2), Leaf)

    let triple =
      Tree (Tree (Leaf, ("x", 1), Leaf), ("y", 2), Tree (Leaf, ("z", 3), Leaf))

    let%test "btree insert" = insert ("y", 2, empty) = single
    let%test "btree insert2" = insert ("x", 1, single) = double
    let%test "btree insert3" = insert ("z", 3, double) = triple
    let%test "btree member" = member "a" empty = false
    let%test "btree member2" = member "x" triple = true
    let%test "btree lookup" = lookup "y" triple = Some 2
    let%test "btree lookup2" = lookup "x" empty = None
  end)

(* NOTE: A nice tutorial on Red-black trees -- what I think the answer to #5 should be -- https://cs3110.github.io/textbook/chapters/ds/rb.html *)

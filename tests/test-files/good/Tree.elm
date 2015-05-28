module Tree where

type Tree a 
    = Node (Tree a) (Tree a)
    | Leaf a

map : (a -> b) -> Tree a -> Tree b
map f tree = case tree of
    Node left right -> Node (map f left) (map f right)
    Leaf x -> Leaf (f x)

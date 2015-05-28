module Tree where

type Tree a 
    = Node { left : Tree a, right : Tree a }
    | Leaf a

map : (a -> b) -> Tree a -> Tree b
map f tree = case tree of
    Node { left, right } -> Node 
        { left = map f left
        , right = map f right }
    Leaf x -> Leaf (f x)
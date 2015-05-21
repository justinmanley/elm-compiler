module Tree where

type Tree a 
    = Node { left : Tree a, value : a, right : Tree a }
    | Leaf a

map : (a -> b) -> Tree a -> Tree b
map f tree = case tree of
    Node { left, value, right } -> Node 
        { left = map f left
        , value = f value
        , right = map f right }
    Leaf x -> Leaf (f x)
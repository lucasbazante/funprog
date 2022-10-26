module Trees where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving ( Show, Eq )

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node x left right) = flatten left ++ [x] ++ flatten right

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x) = Leaf (f x)
tmap f (Node x left right) = Node (f x) (tmap f left) (tmap f right)

module MyNat where

data Nat = Zero 
         | Succ Nat
         deriving (Show)

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = Succ (plus n m)

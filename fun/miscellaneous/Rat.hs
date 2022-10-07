module Rat 
   ( rat ) where

data Rat = Rat Integer Integer
         deriving ( Show )

rat :: Integer -> Integer -> Rat
rat x y
   | y == 0    = error "Division by zero!"
   | otherwise = Rat x y

instance Eq Rat where
   Rat x y == Rat n m = x * m == y * n

module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter p = concat . map box
    where box x =
            if p x
            then [x]
            else []

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted []         = True
sorted all@(_:xs) = (and . zipWith (<=) all) xs

-- using zipWith
fibs :: Integral i => [i]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

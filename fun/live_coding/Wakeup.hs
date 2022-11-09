module Wakeup where

import Data.List

insertAt :: a -> Int -> [a] -> [a]
insertAt y _ [] = [y]
insertAt y n (x:xs)
    | n <= 0    = y:x:xs
    | otherwise = x : insertAt y (n - 1) xs

inserts :: a -> [a] -> [[a]]
inserts x xs = zipWith (++) (inits xs) ((x:) <$> tails xs)

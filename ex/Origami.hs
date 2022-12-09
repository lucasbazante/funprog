module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , reverse
    , length
    , concat
    , filter
    , minimum
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _ v []       = v
foldr (#) v (x:xs) = x # foldr (#) v xs

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v []       = v
foldl (#) v (x:xs) = foldl (#) (v # x) xs

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ []       = error "Empty list!"
foldr1 _ [x]      = x
foldr1 (#) (x:xs) = x # foldr1 (#) xs

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ []         = error "Empty list!"
foldl1 (#) (x:y:xs) = foldl (#) x xs


--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ v []       = [v]
scanl (#) v (x:xs) = v : scanl (#) (v # x) xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ v []       = [v]
scanr (#) v (x:xs) = x # (head rest) : rest
    where rest = scanr (#) v xs

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldr (+) 0 

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr ((||).p) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr ((&&).p) True

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

minimum :: Ord a => [a] -> a
minimum = foldr1 min

maximum :: Ord a => [a] -> a
maximum = foldr1 max

length :: Integral i => [a] -> i
length = foldr (\_ y -> y + 1) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr ((++).box) []
    where box x = if p x then [x] else []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((++).boxf) []
    where boxf x = [f x]

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) [] 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined


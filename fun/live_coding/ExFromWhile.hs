module ExFromWhile where

-- begin in position specified by Int and then take while predicate is true
fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n p = takeWhile p . drop n     -- drop the first n then takeWhile 

-- begin in position specified by Int1 and return the next Int2 elements from
-- list
fromFor :: Int -> Int -> [a] -> [a]
fromFor n m = take m . drop n         -- drop the first n and take the next m

-- begin in position Int1 and take until position Int2
fromTo :: Int -> Int -> [a] -> [a]
fromTo n m = take (m - n + 1) . drop n
-- drop the first n then take until index m, or fromFor n (m - n + 1)

-- in the index interval [Int1, Int2] in the list, take elements which
-- satisfy the predicate
fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat n m p = filter p . fromTo n m
-- take the interval [n, m] then filter for the predicate

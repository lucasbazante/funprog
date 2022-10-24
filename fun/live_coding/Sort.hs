module Sort 
    ( sort
    , msort
    , qsort
    , isort
    ) where

-- default sorting functin is merge sort
sort :: Ord a => [a] -> [a]
sort = msort

-- {{{ merge sort

msort :: Ord a => [a] -> [a]
msort []  = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys)
    where
        (xs, ys) = halve zs

-- ASSUMPTION: xs and ys are sorted!
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge lx@(x:xs) ly@(y:ys)
    | x <= y     = x : merge xs ly
    | otherwise  = y : merge lx ys

-- we dont need the exact midpoint to split!
halve :: [a] -> ([a], [a])
halve []       = ([], [])
halve [x]      = ([x], [])
halve (x:y:xs) = (x:lxs, y:rxs)
    where
        (lxs, rxs) = halve xs

-- }}}


-- {{{ insertion sort

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- ASSUMPTION: for insert x ys:
--             ys is sorted
insert :: Ord a => a -> [a] -> [a]
insert x []         = [x]
insert x all@(y:ys) = 
    if x < y
    then x : all
    else y : insert x ys

-- }}}


-- {{{ quick sort

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (w:xs) = qsort small ++ [w] ++ qsort large
    where
        (small, large) = partition (<w) xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ []  = ([], [])
partition p (x:xs) =
    if p x
    then (x : lxs, rxs)
    else (lxs, x : rxs)
    where
        (lxs, rxs) = partition p xs

-- }}}


-- {{{ tests

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _ = True

prop_qsortLenght xs = length xs == (length . qsort) xs
prop_qsortSorted xs = (sorted . qsort) xs
prop_qsortQsort  xs = qsort xs == (qsort . qsort) xs

-- }}}

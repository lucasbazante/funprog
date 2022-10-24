module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head []    = error "Head of an empty list is illegal!"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "Tail of an empty list is illegal!"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (x:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = append x (reverse xs)

append :: a -> [a] -> [a]
append x []     = [x]
append x (y:ys) = y : append x ys

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc e []     = [e]
snoc e (x:xs) = x : snoc e xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []       = error "Minimum of an empty list is illegal!"
minimum [x]      = x
minimum (x:y:xs) = 
    if x < y 
    then minimum (x:xs) 
    else minimum (y:xs)

maximum :: Ord a => [a] -> a
maximum []       = error "Maximum of an empty list is illegal!"
maximum [x]      = x
maximum (x:y:xs) = 
    if x > y 
    then maximum (x:xs) 
    else maximum (y:xs)

take :: Integral i => i -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

drop :: Integral i => i -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (x:xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) =
    if p x 
    then x : takeWhile p xs 
    else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) =
    if p x 
    then dropWhile p xs 
    else x:xs

tails :: [a] -> [[a]]
tails []         = [[]]
tails lst@(_:xs) = lst : tails xs

-- init

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

-- results not exactly equal (in order) to the original but meh
subsequences :: [a] -> [[a]]
subsequences []     = [[]]
subsequences (x:xs) = (subsequences xs) ++ map (x:) (subsequences xs)

any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs) = p x && all p xs

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or []     = False
or (x:xs) = x || or xs

-- (++) works because x is a list!
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs 

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem = any . (==) -- point-free looks nice

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) = e == x || elem' e xs 

(!!) :: Integral i => [a] -> i -> a
xs !! n
    | n < 0     = error "Negative index is illegal!"
    | otherwise = xs !!! n
    where []     !!! _ = error "Index too large!"
          (x:xs) !!! 0 = x
          (_:xs) !!! n = xs !!! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) =
    if p x
    then x : filter p xs
    else filter p xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ []         = ([], [])
break p all@(x:xs)
    | p x       = ([], xs)
    | otherwise = 
        let (ys, zs) = break p xs
        in  (x:ys, zs)

-- lines
words :: String -> [String]
words [] = []
words xss@(x:xs)
    | C.isSpace x = words xs
    | otherwise   = ys : words zs
        where (ys, zs) = break (C.isSpace) xss

-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}


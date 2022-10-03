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
head xs = case xs of []    -> error "Head of an empty list is illegal!"
                     (x:_) -> x

tail :: [a] -> [a]
tail xs = case xs of []     -> error "Tail of an empty list is illegal!"
                     (_:xs) -> xs

null :: [a] -> Bool
null xs = case xs of [] -> True
                     _  -> False

length :: Integral i => [a] -> i
length xs = case xs of []     -> 0
                       (x:xs) -> 1 + length xs

sum :: Num a => [a] -> a
sum xs = case xs of []    -> 0
                    (x:xs) -> x + sum xs

product :: Num a => [a] -> a
product xs = case xs of []     -> 1
                        (x:xs) -> x * product xs

reverse :: [a] -> [a]
reverse xs = case xs of []     -> []
                        (x:xs) -> append x (reverse xs)

append :: a -> [a] -> [a]
append x ys = case ys of []     -> [x]
                         (y:ys) -> y : append x ys

(++) :: [a] -> [a] -> [a]
xs ++ ys = case xs of []     -> ys
                      (x:xs) -> x : xs ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc e xs = case xs of []     -> [e]
                       (x:xs) -> x : snoc e xs

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
minimum xs = case xs of []       -> error "Minimum of an empty list is illegal!"
                        [x]      -> x
                        (x:y:xs) -> if x < y then minimum (x:xs) else minimum (y:xs)

maximum :: Ord a => [a] -> a
maximum xs = case xs of []       -> error "Maximum of an empty list is illegal!"
                        [x]      -> x
                        (x:y:xs) -> if x > y then maximum (x:xs) else maximum (y:xs)

take :: Integral i => i -> [a] -> [a]
take n xs = case (n, xs) of (_, [])     -> []
                            (0, _)      -> []
                            (n, (x:xs)) -> x : take (n - 1) xs

drop :: Integral i => i -> [a] -> [a]
drop n xs = case (n, xs) of (_, [])     -> []
                            (0, xs)     -> xs
                            (n, (x:xs)) -> drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = case xs of []     -> []
                            (x:xs) -> if p x 
                                      then x : takeWhile p xs 
                                      else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = case xs of []       -> []
                            (x:xs) -> if p x 
                                      then dropWhile p xs 
                                      else x:xs

tails :: [a] -> [[a]]
tails xs = case xs of []         -> [[]]
                      lst@(_:xs) -> lst : tails xs

-- init

inits :: [a] -> [[a]]
inits xs = case xs of []     -> [[]]
                      (x:xs) -> [] : map (x:) (inits xs)

-- results not exactly equal (in order) to the original but meh
subsequences :: [a] -> [[a]]
subsequences xs = case xs of []     -> [[]]
                             (x:xs) -> (subsequences xs) ++ map (x:) (subsequences xs)

any :: (a -> Bool) -> [a] -> Bool
any f xs = case xs of []     -> False
                      (x:xs) -> if f x then True else any f xs

all :: (a -> Bool) -> [a] -> Bool
all f xs = case xs of []     -> True
                      (x:xs) -> f x && all f xs

and :: [Bool] -> Bool
and xs = case xs of []     -> True
                    (x:xs) -> x && and xs

or :: [Bool] -> Bool
or xs = case xs of []     -> False
                   (x:xs) -> x || or xs

-- (++) works because x is a list!
concat :: [[a]] -> [a]
concat xs = case xs of [] -> []
                       (x:xs) -> x ++ concat xs 

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem = any . (==) -- point-free looks nice

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' e xs = case xs of []     -> False
                        (x:xs) -> if e == x then True else elem' e xs

(!!) :: Integral i => [a] -> i -> a
xs !! n
    | n < 0     = error "Negative index is illegal!"
    | otherwise = xs !!! n
    where []     !!! _ = error "Index too large!"
          (x:xs) !!! 0 = x
          (_:xs) !!! n = xs !!! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [ x | x <- xs, p x ]

map :: (a -> b) -> [a] -> [b]
map f xs = case xs of []     -> []
                      (x:xs) -> f x : map f xs

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

-- break

-- lines
-- words
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


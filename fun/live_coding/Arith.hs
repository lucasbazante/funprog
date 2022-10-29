module Arith where

data Arith = Atom Integer
           | Plus Arith Arith
           | Times Arith Arith

instance Show Arith where
    show (Atom n)    = show n
    show (Plus a b)  = parenthesize $ showop " + " a b
    show (Times a b) = parenthesize $ showop " * " a b

showop op a b    = show a ++ op ++ show b
parenthesize exp = "(" ++ exp ++ ")"

val :: Arith -> Integer
val (Atom n)    = n
val (Plus a b)  = val a + val b
val (Times a b) = val a * val b

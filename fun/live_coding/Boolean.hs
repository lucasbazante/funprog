module Boolean where

data Boolean = F
             | T

lor :: Boolean -> Boolean -> Boolean
lor T _ = T
lor F x = x

land :: Boolean -> Boolean -> Boolean
land T x = x
land F _ = F

-- addings after the live coding

lnot :: Boolean -> Boolean
lnot T = F
lnot F = T

ifthenelse :: Boolean -> a -> a -> a
ifthenelse T x _ = x
ifthenelse F _ y = y

instance (Show Boolean) where
    show T = "T"
    show F = "F"

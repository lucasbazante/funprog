module Shapes where

data Shape = Circle Double
           | Square Double
           | Rectangle Double Double
           deriving (Show, Eq)

data Pair a b = Pair a b
              deriving (Show, Eq)

outl :: Pair a b -> a
outl (Pair x y) = x

outr :: Pair a b -> b
outr (Pair x y) = y

surface :: Shape -> Double
surface (Circle r)      = pi * (r^2)
surface (Square s)      = s^2
surface (Rectangle w h) = w * h

rot90 :: Shape -> Shape
rot90 (Rectangle w h) = Rectangle h w
rot90 x               = x

height :: Shape -> Double
height (Circle r)      = 2 * r 
height (Square s)      = s
height (Rectangle _ h) = h

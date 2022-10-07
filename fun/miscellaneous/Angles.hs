module Angles where

data Angle = Angle  Int

instance Eq Angle where
   Angle x == Angle y = 360 `divides` ( x - y )

divides :: Int -> Int -> Bool
x `divides` y = y `mod` x == 0

isTriangle :: Angle -> Angle -> Angle -> Bool
isTriangle (Angle x) (Angle y) (Angle z) = sum [x, y, z] == 180

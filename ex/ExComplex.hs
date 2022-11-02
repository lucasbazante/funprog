module ExComplex
    ( complex
    , conjugate
    , toRect
    , toTrig
    , magnitude
    , arg
    ) where

import Numeric (showFFloat)

type RePart    = Double
type ImPart    = Double
type Magnitude = Double
type Angle     = Double     -- in rads!

data Complex = Rect RePart    ImPart
             | Trig Magnitude Angle

showf :: RealFloat a => a -> String -> String
showf = showFFloat (Just 2)

instance Show Complex where
    show (Rect a b) = showf a " + " ++ "(" ++ showf b ")i"
    show (Trig r s) = showf r "(cis " ++ showf s ")"
    --show (Trig r s) = showf r "(cos " ++ showf s " + i(sin " ++ showf s "))"
        
instance Eq Complex where
    (Rect a b) == (Rect c d) = a == c && b == d
    (Trig r s) == (Trig t u) = r == t && s == u

instance Num Complex where
    (Rect a b) + (Rect c d)      = Rect (a + c) (b + d)
    z@(Rect _ _) + z'@(Trig _ _) = z + toRect z'
    z@(Trig _ _) + z'@(Trig _ _) = toTrig $ toRect z + toRect z'
    z@(Trig _ _) + z'@(Rect _ _) = toRect z + z'
    
    (Rect a b) * (Rect c d)      = Rect (a*c - b*d) (a*d + b*c)
    z@(Rect _ _) * z'@(Trig _ _) = z * toRect z'
    (Trig r s) * (Trig t u)      = Trig (r * t) (s + u)
    z@(Trig _ _) * z'@(Rect _ _) = z * toTrig z'

    negate (Rect a b) = Rect (-a) (-b)
    negate (Trig r s) = Trig (-r) s

    abs z@(Rect a b) = Rect (magnitude z) 0
    abs (Trig r _)   = Rect r 0 

    signum (Rect 0 0)   = Rect 0 0
    signum z@(Rect a b) = Rect (a/r) (b/r)
        where r = magnitude z
    signum z@(Trig _ _) = signum $ toRect z
    
    fromInteger n = Rect (fromInteger n) 0

complex :: RePart -> ImPart -> Complex
complex a b = Rect a b

conjugate :: Complex -> Complex
conjugate (Rect a b) = Rect a (-b)
conjugate (Trig r s) = Trig r (-s)

toRect :: Complex -> Complex
toRect z@(Rect a b) = z
toRect (Trig r s)   = Rect a b
    where a = r * cos s
          b = r * sin s

toTrig :: Complex -> Complex
toTrig z@(Trig _ _) = z
toTrig z@(Rect a b) = Trig r s
    where
        r = magnitude z
        s = atan (b/a) + if a > 0 then 0 else pi

magnitude :: Complex -> Magnitude
magnitude (Trig r _) = r
magnitude (Rect a b) = sqrt (a*a + b*b)

-- arg should be a number in (-pi, pi]
arg :: Complex -> Angle
arg (Trig _ s)   = s
arg z@(Rect _ _) = arg $ toTrig z

-- real part
re :: Complex -> RePart
re (Rect a _)   = a
re z@(Trig _ _) = re $ toRect z

-- imaginary part
im :: Complex -> ImPart
im (Rect _ b)   = b
im z@(Trig _ _) = im $ toRect z

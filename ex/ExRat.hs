module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

data Rat = Rat Integer Integer 

instance Show Rat where
    show (Rat p q) = show p ++ "/" ++ show q

instance Eq Rat where
    (Rat p q) == (Rat r s) = (p * s) == (q * r)

-- I can abuse the fact that Rat is defined by two Integers!
-- I'll assume Rat p q is never used as constuctor, but always
-- rat p q
instance Num Rat where
    (Rat p q) + (Rat r s) = rat (p * s + q * r) (q * s)

    (Rat p q) * (Rat r s) = rat (p * r) (q * s)   
    
    negate (Rat p q) = rat (negate p) q

    abs (Rat p q) = rat (abs p) q

    signum (Rat p q) = rat (signum p) 1

    fromInteger n = rat n 1

instance Ord Rat where
    compare (Rat p q) (Rat r s) = compare (p * s) (q * r) 

rat :: Integer -> Integer -> Rat
rat p q
    | q == 0         = error "Zero in the denominator is illegal!"
    | p < 0 && q < 0 = rat (-p) (-q)    -- -a/-b = a/b
    | otherwise      = Rat ( p `div` gd) ( q `div` gd)
                     where gd = gcd p q     -- minimizing fractions :)

(//) :: Rat -> Rat -> Rat
_         // (Rat 0 _) = error "Division by zero is illegal!"
(Rat p q) // (Rat r s) = rat (p * s) (q * r)

denominator :: Rat -> Integer
denominator (Rat _ q) = q

numerator :: Rat -> Integer
numerator (Rat p _) = p

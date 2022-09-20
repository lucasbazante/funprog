module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero     = "O" 
    show (Succ n) = 'S' : show n

instance Eq Nat where
    (==) Zero Zero         = True
    (==) Zero _            = False
    (==) _ Zero            = False
    (==) (Succ n) (Succ m) = (n == m)

instance Ord Nat where
    (<=) Zero _            = True
    (<=) _ Zero            = False
    (<=) (Succ n) (Succ m) = (n <= m)

    min Zero _ = Zero
    min _ Zero = Zero
    min (Succ n) (Succ m) = Succ$min n m

    max Zero n = n
    max n Zero = n
    max (Succ n) (Succ m) = Succ$max n m

isZero :: Nat -> Bool
isZero n
    | n == Zero = True
    | otherwise = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ n)) = even n

odd :: Nat -> Bool
odd n = not$even n

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero n     = n
(<+>) n Zero     = n
(<+>) (Succ n) m = n <+> (Succ m) -- trivial

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _            = Zero
(<->) n Zero            = n
(<->) (Succ n) (Succ m) = n <-> m -- evenerything goes down until one reaches zero

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) Zero _        = Zero
(<*>) _ Zero        = Zero
(<*>) n (Succ Zero) = n
(<*>) (Succ n) m    = m <+> (n <*> m) -- adds m and multiplies again by predecessor of n+1 (n), recursively building product

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero        = (Succ Zero)
(<^>) Zero _        = Zero
(<^>) n (Succ Zero) = n
(<^>) n (Succ m)    = n <*> n <^> m

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) _ Zero        = error "Division by zero is illegal!"
(</>) Zero _        = Zero
(</>) n (Succ Zero) = n
(</>) n m
    | n < m  = Zero
    | n == m = (Succ Zero)
    | n > m  = (Succ Zero) <+> ((n <-> m) </> m) -- subtracts m from n and divide again, adding one (recursively building quotient)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ Zero = error "Division by zero is illegal!"
(<%>) Zero _ = Zero
(<%>) n (Succ Zero) = Zero
(<%>) n m
    | n < m  = n
    | n == m = Zero
    | n > m  = n <-> m <%> m -- subtracts m from n, takes modulo again, eventually reaching a base case

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n m = (n <%> m) == Zero

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m
    | n > m  = n <-> m
    | n == m = Zero
    | n < m  = m <-> n

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero        = (Succ Zero)
factorial (Succ Zero) = (Succ Zero)
factorial (Succ n)    = (Succ n) <*> (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = (Succ Zero) -- no negative numbers in Nat

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ Zero        = error "Log of zero is illegal!"
lo Zero _        = error "Log base zero is illegal!"
lo n (Succ Zero) = n
lo n m
    | n > m  = Zero
    | n == m = (Succ Zero)
    | n < m  = (Succ Zero) <+> lo n (m </> n) -- keep dividing each factor of n

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = 0
toNat n = Succ$toNat (n - 1)

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n -- plus one is same as Succ!


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "Negative Natural is illegal!"
        | otherwise = toNat x -- case x == 0 is covered


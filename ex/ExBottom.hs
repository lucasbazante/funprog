module ExBottom where

{- For each of the types we have encountered so far
 - (1) how many values does this type have?
 - (2) describe all its values.
 - Create contexts to demonstrate your claims
 - regarding which values are really distinct within
 - each type.
 -}

bottom :: a
bottom = bottom

-- Bool
-- Bool has 3 values:
-- False, True, bottom
-- To show they are distinct with v :: Bool:
-- define:
bottomBool :: Bool -> Int
bottomBool False  = 0
bottomBool True   = 1
-- check: bottomBool v

-- Any of (choose one): Int, Char, Double
-- Double
-- Double has 

-- Integer

-- Nat

-- Box α

-- Tuples: (α,β)

-- List α

module Funktor where

import Prelude hiding 
  ( fmap
  , (<$)
  , (<$>)
  , Functor(..)
  , Applicative(..)
  , (<*>)
  , (<*)
  , (*>)
  , liftA, liftA2, liftA3
  )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const

instance Funktor [] where
  fmap = map

instance Funktor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just $ f x

instance Funktor (Either a) where
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right $ f x

instance Funktor IO where
  fmap f ax =
    do x <- ax
       return $ f x

instance Funktor ((,) e) where
  fmap f (x, y) = (x, f y)

instance Funktor ((->) r) where
  fmap = (.)

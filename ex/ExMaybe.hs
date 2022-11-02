module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes []              = []
catMaybes (x:xs)  = case x of
    Nothing -> catMaybes xs
    Just x  -> x : catMaybes xs

fromJust :: Maybe a -> a
fromJust Nothing  = error "fromJust Nothing is illegal!"
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing  = y
fromMaybe y (Just x) = x

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
    Nothing -> mapMaybe f xs
    Just x  -> x : mapMaybe f xs

maybe :: b -> (a -> b) -> Maybe a -> b
maybe y _ Nothing  = y
maybe y f (Just x) = f x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

maybeApply :: Maybe (a -> a) -> a -> a
maybeApply Nothing x  = x
maybeApply (Just f) x = f x

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = zipWith maybeApply

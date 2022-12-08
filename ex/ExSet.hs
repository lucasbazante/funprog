module ExSet
    ( Set
    , empty
    , singleton
    , fromList
    , toList
    , powerSet
    , insert
    , delete
    , member
    , notMember
    , setNull
    , size
    , isSubsetOf
    , isProperSubsetOf
    , disjoint
    , pairwiseDisjoint
    , union
    , inter
    , (\\)
    , unions
    , inters
    , cartesianProduct
    , disjointUnion
    , setFilter
    , partition
    , setMap
    ) where

import qualified Data.List as L

data Set a = Set [a]

-- CAUTION: you may need to add constraints to your types and instances!

instance Eq a => Eq (Set a) where
    (Set xs) == (Set ys) = xs == ys

instance Show a => Show (Set a) where
    show (Set xs) = "{" ++ elements xs ++ "}"
        where
            elements = L.intercalate "," . map show

instance Foldable Set where
    foldr f z = foldr f z . toList
    foldl f z = foldl f z . toList

-- smart constructor
set :: Eq a => [a] -> Set a
set = fromList

empty :: Set a
empty = Set []

singleton :: a -> Set a
singleton x = Set [x]

fromList :: Eq a => [a] -> Set a
fromList []     = empty
fromList (x:xs) = singleton x `union` fromList xs

toList :: Set a -> [a]
toList (Set xs) = xs

powerSet :: Set a -> Set (Set a)
powerSet = undefined

insert :: Ord a => a -> Set a -> Set a
insert x = set . L.insert x . toList

delete :: Eq a => a -> Set a -> Set a
delete x = set . L.delete x . toList

member :: Eq a => a -> Set a -> Bool
member x = L.elem x . toList

notMember :: Eq a => a -> Set a -> Bool
notMember x = not . member x

setNull :: Set a -> Bool
setNull = L.null . toList

size :: Set a -> Int
size = L.length . toList

isSubsetOf :: Eq a => Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = L.isSubsequenceOf xs ys

isProperSubsetOf :: Eq a => Set a -> Set a -> Bool
isProperSubsetOf xs ys = isSubsetOf xs ys && not (xs == ys)

disjoint :: Eq a => Set a -> Set a -> Bool
disjoint xs ys = inter xs ys == empty

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint = undefined

union :: Eq a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set $ L.union xs ys

inter :: Eq a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set $ L.intersect xs ys

-- relative complement (set difference)
setminus :: Eq a => Set a -> Set a -> Set a
setminus (Set xs) (Set ys) = Set $ xs L.\\ ys

(\\) :: Eq a => Set a -> Set a -> Set a
(\\) = setminus
infixr 5 \\

unions :: Eq a => Set (Set a) -> Set a
unions = foldl union empty

inters :: Eq a => Set (Set a) -> Set a
inters xs = foldl inter first xs
    where first = (L.head . toList) xs

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = undefined

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = undefined

setFilter :: Eq a => (a -> Bool) -> Set a -> Set a
setFilter p = set . filter p . toList

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

setMap :: Eq b => (a -> b) -> Set a -> Set b
setMap f = set . map f . toList

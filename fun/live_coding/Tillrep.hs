module Tillrep where

tillRep :: Eq a => [a] -> [a]
tillRep []       = []
tillRep [x]      = [x]
tillRep (x:y:xs) =
    if x == y
    then [x, y]
    else x : tillRep (y:xs)

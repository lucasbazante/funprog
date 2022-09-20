module RecList where

(+++) :: [a] -> [a] -> [a]
xs +++ ys = case xs of []     -> ys
                       (x:xs) -> x:(xs ++ ys)

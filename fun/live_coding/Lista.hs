module Lista where

data ListInt = Empty
             | ListInt Int ListInt

instance (Show ListInt) where
  show h = "[" ++ show' h ++ "]" where
    show' :: ListInt -> [Char]
    show' Empty                = ""
    show' (ListInt h Empty)    = show h
    show' (ListInt h t)        = show h ++ ", " ++ show' t


hd :: ListInt -> Int
hd Empty = error "empty list!"
hd (ListInt h _) = h

tl :: ListInt -> ListInt
tl Empty = error "empty list!"
tl (ListInt _ t) = t

module Sem where

import Prelude hiding ( putStr )

putStr' :: String -> IO ()
putStr' = sequence_ . map putChar

putStr :: String -> IO ()
putStr "" = skip
putStr (c:cs) =
    do putChar c
       putStr cs

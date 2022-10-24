module Modernise where

import Data.Char

modernise :: String -> String
modernise = unwords . map (\(x:xs) -> toUpper x : xs) . words

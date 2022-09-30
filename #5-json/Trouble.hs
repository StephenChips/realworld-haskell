module Trouble where

import Data.Char

{-
-- Fogot :cs and empty cases here
upcaseFirst (c:cs) = toUpper c

camelCase xs = concatMap upcaseFirst (words xs)
-}

upcaseFirst :: String -> String
upcaseFirst [] = []
upcaseFirst (c:cs) = toUpper c : cs

camelCase :: String -> String
camelCase xs = concatMap upcaseFirst (words xs)
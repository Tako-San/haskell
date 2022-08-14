module ConcatList where

concatList :: [[a]] -> [a]
concatList = foldr (++) []

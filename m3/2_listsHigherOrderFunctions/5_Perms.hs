module Perms
  ( perms
  ) where

perms :: [a] -> [[a]]

perms []       = [[]]
perms [x     ] = [[x]]
perms (x : xs) = concatMap (insert 0 x) (perms xs) where
  insert i x xs | length xs == i = [xs ++ [x]]
                | otherwise      = insertAt i x xs : insert (i + 1) x xs
    where insertAt n x xs = let (ys, zs) = splitAt n xs in ys ++ [x] ++ zs

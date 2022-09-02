module Avg where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving(Show)

avg :: Tree Int -> Int
avg t = let (c, s) = go t in s `div` c
 where
  go :: Tree Int -> (Int, Int)
  go (Leaf val) = (1, val)
  go (Node l r) =
    let (cl, sl) = go l
        (cr, sr) = go r
    in  (cl + cr, sl + sr)

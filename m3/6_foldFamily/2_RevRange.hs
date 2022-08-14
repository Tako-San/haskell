module RevRange where

import Data.List (unfoldr)

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g (from, to) | from > to = Nothing
                     | otherwise = Just (to, (from, pred to))

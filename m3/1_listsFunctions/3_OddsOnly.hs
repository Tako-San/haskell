module OddsOnly
  ( oddsOnly
  , oddsOnly'
  ) where

oddsOnly :: Integral a => [a] -> [a]
oddsOnly lst' = reverse $ helper lst' [] where
  helper lst res | null lst       = res
                 | odd $ head lst = helper (tail lst) (head lst : res)
                 | otherwise      = helper (tail lst) res

oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' [] = []
oddsOnly' (x : xs) | odd x     = x : oddsOnly' xs
                   | otherwise = oddsOnly' xs

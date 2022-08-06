module SumNCount where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x >= 0 = helper x
              | x < 0  = helper (-x)
  where
    helper x =

module NTimes
  ( nTimes, nTimes'
  ) where

nTimes :: a -> Int -> [a]
nTimes elem times = helper times []
 where
  helper n lst | n == 0    = lst
               | otherwise = helper (n - 1) (elem : lst)

nTimes' :: a -> Int -> [a]
nTimes' elem times | times == 0 = []
                   | otherwise  = elem : nTimes' elem (times - 1)

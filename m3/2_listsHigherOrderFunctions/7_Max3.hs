module Max3
  ( max3
  , max3'
  ) where

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 mx where mx a b c = a `max` b `max` c

max3' :: Ord a => [a] -> [a] -> [a] -> [a]
max3' = zipWith3 (\a b c -> a `max` b `max` c)


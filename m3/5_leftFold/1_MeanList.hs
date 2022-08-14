module MeanList where

meanList :: [Double] -> Double
meanList lst =
  let (s, c) = foldr (\x (s', c') -> (x + s', c' + 1)) (0, 0) lst in s / c

meanList' :: [Double] -> Double
meanList' = uncurry (/) . foldr (\x (s, c) -> (x + s, c + 1)) (0, 0)

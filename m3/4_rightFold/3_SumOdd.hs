module SumOdd where

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

sumOdd' :: [Integer] -> Integer
sumOdd' = foldr (\x s -> (x `mod` 2) * x + s) 0

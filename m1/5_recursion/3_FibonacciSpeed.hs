module FibonacciSpeed where

calcSign n = if (n `mod` 2) == 0 then -1 else 1

fibPos t1 t2 n | n == 0    = t1
               | n == 1    = t2
               | n > 0     = fibPos t2 (t1 + t2) (n - 1)
               | otherwise = error "n should be positive in helper function"

fibonacci :: Integer -> Integer
fibonacci n | n >= 0   = fibPos 0 1 n
            | n < 0    = calcSign(-n) * fibonacci(-n)

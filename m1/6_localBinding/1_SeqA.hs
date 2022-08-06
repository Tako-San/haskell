module SeqA where

seqA :: Integer -> Integer
seqA n | n < 0     = error "arg should be positive"
       | otherwise = let
           helper t1 t2 t3 0 = t1
           helper t1 t2 t3 1 = t2
           helper t1 t2 t3 2 = t3
           helper t1 t2 t3 n = helper t2 t3 (t3 + t2 - 2 * t1) (n - 1)
         in helper 1 2 3 n

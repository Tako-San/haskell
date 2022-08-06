module DoubleFact where

-- 7!! = 7 * 5 * 3 * 1
-- 8!! = 8 * 6 * 4 * 2
doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact(n - 2)

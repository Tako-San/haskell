module LengthList where

lengthList :: [a] -> Int
lengthList = foldr (\x l -> succ l) 0

lengthList' :: [a] -> Int
lengthList' = foldr (\_ l -> succ l) 0

-- const succ x l == (const succ x) l == succ l
lengthList'' :: [a] -> Int
lengthList'' = foldr (const succ) 0

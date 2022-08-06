module SumNCount where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x >= 0    = (sumDigits x, countDigits x)
              | x < 0     = (sumDigits (-x), countDigits (-x))
              | otherwise = undefined
 where
  sumDigits num = sdhelper 0 num

  sdhelper acc num | num == 0  = acc
                   | num > 0   = sdhelper (acc + num `mod` 10) (num `div` 10)
                   | otherwise = undefined

  countDigits num | num == 0  = 1
                  | num > 0   = cdhelper 0 num
                  | otherwise = undefined

  cdhelper counter num | num == 0  = counter
                       | num > 0   = cdhelper (counter + 1) (num `div` 10)
                       | otherwise = undefined

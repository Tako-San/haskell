module Sum3 where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 as bs cs = hd as + hd bs + hd cs : sum3 (tl as) (tl bs) (tl cs) where
  hd lst | null lst  = 0
         | otherwise = head lst
  tl lst | null lst  = []
         | otherwise = tail lst

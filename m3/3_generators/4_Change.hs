module Change where

-- coins :: [(Ord a, Num a) => [a]]
coins :: Num a => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change s | s < minimum coins = []
         | otherwise         = helper s   where
      helper s'
             | s' < minimum coins
             = [[]]
             | otherwise
             = [ x : xs | x <- coins, xs <- helper (s' - x), sum xs == s' - x ]

change' :: (Ord a, Num a) => a -> [[a]]
change' s | s == 0            = [[]]
          | s < minimum coins = []
          | otherwise         = [ x : xs | x <- coins, xs <- change' (s - x) ]




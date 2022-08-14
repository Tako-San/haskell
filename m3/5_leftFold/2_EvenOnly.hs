module EvenOnly where

evenOnly :: [a] -> [a]
evenOnly = fst . foldl
  (\(l, i) x -> if even i then (l ++ [x], i + 1) else (l, i + 1))
  ([], 1)

evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a (xs, ys) -> (a : ys, xs)) ([], [])

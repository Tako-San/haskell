module EvenOnlyInf where

evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])

-- `~` needed for lazy pattern matching

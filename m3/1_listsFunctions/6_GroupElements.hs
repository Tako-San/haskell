module GroupElements where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems as = let (bs, cs) = break (/= head as) as in bs : groupElems cs

groupElems' :: Eq a => [a] -> [[a]]
groupElems' as' = reverse $ helper as' [] where
  helper [] res = res
  helper as res = let (bs, cs) = break (/= head as) as in helper cs (bs : res)

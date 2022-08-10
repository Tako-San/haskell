module FilterDisj
  ( filterDisj
  , filterDisj'
  ) where

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter $ disj p1 p2 where disj f1 f2 arg = f1 arg || f2 arg

filterDisj' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj' p1 p2 = filter $ \x -> p1 x || p2 x

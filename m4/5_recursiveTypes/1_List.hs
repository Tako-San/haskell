module List where

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil              = []
fromList (Cons head list) = head : fromList list

toList :: [a] -> List a
toList []       = Nil
toList (x : xs) = Cons x $ toList xs

toList' :: [a] -> List a
toList' = foldr Cons Nil

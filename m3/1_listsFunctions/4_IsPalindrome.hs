module IsPalindrome
  ( isPalindrome
  , isPalindrome'
  ) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome lst = isEqual lst $ reverse lst where
  isEqual [] [] = True
  isEqual _  [] = False
  isEqual [] _  = False
  isEqual l1 l2 | head l1 == head l2 = isEqual (tail l1) (tail l2)
                | otherwise          = False


isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' lst = isEqual lst $ reverse lst where
  isEqual (l1 : l1s) (l2 : l2s) | l1 == l2  = isEqual l1s l2s
                                | otherwise = False
  isEqual [] [] = True
  isEqual _  _  = False;

isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' lst = lst == reverse lst

module DelAllUpper
  ( delAllUpper, delAllUpper'
  ) where

import Data.Char ( isUpper, isLower )

delAllUpper :: String -> String
delAllUpper = unwords . delUpper . words where
  delUpper [] = []
  delUpper (x : xs) | all isUpper x = delUpper xs
                    | otherwise     = x : delUpper xs

delAllUpper' :: String -> String
delAllUpper' = unwords. filter (any isLower) . words

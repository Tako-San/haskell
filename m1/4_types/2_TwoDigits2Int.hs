module TwoDigits2Int where

import           Data.Char

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int tens ones = if isDigit tens && isDigit ones
  then 10 * digitToInt tens + digitToInt ones
  else 100

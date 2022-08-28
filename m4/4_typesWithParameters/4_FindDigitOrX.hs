module FindDigitOrX where

import           Data.Char                      ( isDigit )
import           Data.Maybe                     ( fromMaybe )

findDigit :: [Char] -> Maybe Char
findDigit = foldr (\c x -> if isDigit c then Just c else x) Nothing

findDigitOrX :: [Char] -> Char
findDigitOrX a = case findDigit a of
  Just c  -> c
  Nothing -> 'X'

findDigitOrX' :: [Char] -> Char
findDigitOrX' a = Data.Maybe.fromMaybe 'X' (findDigit a)


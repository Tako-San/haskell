module FindDigit where

import Data.Char ( isDigit )

findDigit :: [Char] -> Maybe Char
findDigit = foldr (\c x -> if isDigit c then Just c else x) Nothing

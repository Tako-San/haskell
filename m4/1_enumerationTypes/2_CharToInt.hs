module CharToInt where

charToInt :: Char -> Int
charToInt c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'

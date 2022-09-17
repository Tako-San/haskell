module Token where

import           Data.Char                      ( isDigit )
import           Text.Read                      ( readMaybe )

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asNumber :: String -> Maybe Token
asNumber s = case readMaybe s :: Maybe Int of
    Just n  -> Just $ Number n
    Nothing -> Nothing

asToken :: String -> Maybe Token
asToken []  = Nothing
asToken [s] = if isDigit s
    then asNumber [s]
    else case s of
        '+' -> Just Plus
        '-' -> Just Minus
        '(' -> Just LeftBrace
        ')' -> Just RightBrace
        _   -> Nothing
asToken str@(s : _) = if isDigit s then asNumber str else Nothing

tokenize :: String -> Maybe [Token]
tokenize = foldr f (return []) . words
  where
    f a r = do
        x  <- asToken a
        xs <- r
        return (x : xs)

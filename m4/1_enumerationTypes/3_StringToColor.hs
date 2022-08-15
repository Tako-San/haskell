module StringToColor where

data Color = Red | Green | Blue deriving (Read, Show)

stringToColor :: String -> Color
stringToColor = read

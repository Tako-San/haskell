module LastElem where

lastElem :: [a] -> a
lastElem = foldl1 $ const id

lastElem' :: [a] -> a
lastElem' = foldl1 (\_ ini -> ini)

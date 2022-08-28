module MaybeFromToList where

maybeToList :: Maybe a -> [a]
maybeToList mb = case mb of
  Just val -> [val]
  Nothing  -> []

listToMaybe :: [a] -> Maybe a
listToMaybe (x : xs) = Just x
listToMaybe []       = Nothing

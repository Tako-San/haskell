module ArrowMapLike where

import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing
  lookup key (ArrowMap f) = f key
  insert key val (ArrowMap f) = ArrowMap $ \key' -> if key == key' then Just val else f key'
  delete key (ArrowMap f) = ArrowMap $ \key' -> if key == key' then Nothing else f key'
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)


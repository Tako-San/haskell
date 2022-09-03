module ListMapLike where

import qualified Data.List                     as L
import           Prelude                 hiding ( lookup )

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup key (ListMap lm) = L.lookup key lm
  insert key val lm'@(ListMap lm) = case lookup key lm' of
    Nothing -> ListMap $ (key, val) : lm
    _       -> insert key val $ delete key lm'
  delete key' (ListMap lm) =
    ListMap $ [ (key, val) | (key, val) <- lm, key /= key' ]

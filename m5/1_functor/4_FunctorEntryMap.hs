module FunctorEntryMap where

data Entry k1 k2 v = Entry (k1, k2) v
  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]
  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry key val) = Entry key (f val)

instance Functor (Map k1 k2) where
  fmap f (Map lst) = Map $ fmap (fmap f) lst

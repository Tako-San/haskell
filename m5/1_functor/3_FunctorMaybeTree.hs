module FunctorMaybeTree where

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf v) = Leaf $ fmap f v
  fmap f (Branch l v r) = Branch (fmap f l) (fmap f v) (fmap f r)


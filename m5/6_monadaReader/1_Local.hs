{-# LANGUAGE InstanceSigs #-}
module Local where

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f x = Reader $ f . runReader x

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader $ const x

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader f) (Reader x) = Reader $ \r -> f r $ x r

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \x -> runReader m $ f x

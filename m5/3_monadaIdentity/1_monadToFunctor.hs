module MonadToFunctor where

instance Functor SomeType where
  fmap f x = x >>= (return . f)

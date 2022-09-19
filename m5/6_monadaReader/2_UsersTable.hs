{-# LANGUAGE InstanceSigs #-}
module UsersTable where

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

ask :: Reader r r
ask = Reader id

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  table <- ask
  let samePwd = filter (\(usr, pwd) -> pwd == "123456") table
  return $ map fst samePwd


module MonadLog where

data Log a = Log [String] a
  deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] $ f x

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  let (Log msg1 val1) = f x
      (Log msg2 val2) = g val1
  in  Log (msg1 ++ msg2) val2

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg1 val1) f =
  let (Log msg2 val2) = f val1 in Log (msg1 ++ msg2) val2

instance Functor Log where
  fmap f (Log msg val) = Log msg $ f val

instance Applicative Log where
  pure = returnLog
  Log msg1 f <*> Log msg2 x = Log (msg1 ++ msg2) (f x)

instance Monad Log where
  return = returnLog
  (>>=)  = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList val = foldl (>>=) (return val)

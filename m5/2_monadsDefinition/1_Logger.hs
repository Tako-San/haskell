module Logger where

data Log a = Log [String] a
  deriving (Show, Read)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] $ f x

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  let (Log msg1 val1) = f x
      (Log msg2 val2) = g val1
  in  Log (msg1 ++ msg2) val2

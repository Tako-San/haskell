module BindLog where

data Log a = Log [String] a
  deriving (Show, Read)

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg1 val1) f =
  let (Log msg2 val2) = f val1 in Log (msg1 ++ msg2) val2


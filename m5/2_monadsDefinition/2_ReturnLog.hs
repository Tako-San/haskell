module ReturnLog where

data Log a = Log [String] a
  deriving (Show, Read)

returnLog :: a -> Log a
returnLog = Log []

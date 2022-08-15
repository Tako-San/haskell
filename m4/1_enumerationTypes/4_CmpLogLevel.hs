{-# LANGUAGE StandaloneDeriving #-}
module CmpLogLevel where


data LogLevel = Error | Warning | Info deriving (Show, Read)

deriving instance Eq LogLevel
instance Ord LogLevel where
  Info    <= _     = True

  Warning <= Info  = False
  Warning <= _     = True

  Error   <= Error = True
  Error   <= _     = False


cmp :: LogLevel -> LogLevel -> Ordering
cmp = compare

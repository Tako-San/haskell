module Log where

import           Data.Function                  ( (&) )
import           Data.Time.Clock                ( UTCTime(UTCTime) )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
-- import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving(Show, Read)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , logLevel  :: LogLevel
  , message   :: String
  }
  deriving (Show, Read)

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString le =
  timeToString (le & timestamp)
    ++ ": "
    ++ show (le & logLevel)
    ++ ": "
    ++ (le & message)

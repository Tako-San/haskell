module ReadDigits
  ( readDigits
  ) where

import           Data.Char                      ( isDigit )

readDigits :: String -> (String, String)
readDigits = span isDigit

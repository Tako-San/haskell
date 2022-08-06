module Printable where

class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString x | x         = "true"
             | otherwise = "false"

instance Printable () where
  toString x = "unit type"

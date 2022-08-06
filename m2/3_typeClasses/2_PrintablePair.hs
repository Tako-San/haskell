module PrintablePair where

class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString x | x         = "true"
             | otherwise = "false"

instance Printable () where
  toString x = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (p1, p2) = "(" ++ toString p1 ++ "," ++ toString p2 ++ ")"

module Integration where

integrationRiemann :: (Double -> Double) -> Double -> Double -> Double
integrationRiemann f a b = h * helper a 0
 where
  h = (b - a) / 1000
  helper from acc | abs (b - from) <= 1e-4 = acc
                  | otherwise              = helper (from + h) (acc + f from)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a > b     = -integration f b a
                  | otherwise = h * (helper a 0 0 - 0.5 * (f a + f b))
 where
  n = 1000
  h = (b - a) / n
  helper from acc i | i == n    = acc
                    | otherwise = helper (from + h) (acc + f from) (i + 1)

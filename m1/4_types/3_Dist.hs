module Dist where

sumSquares x y = x^2 + y^2

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ sumSquares (fst p1 - fst p2) (snd p1 - snd p2)

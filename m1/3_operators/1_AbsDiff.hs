module AbsDiff where

infix |-|
(|-|) x y = if x > y then x - y else y - x

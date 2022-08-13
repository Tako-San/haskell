module RepeatHelper where

repeat :: a -> [a]
repeat = iterate repeatHelper

repeatHelper :: p -> p
repeatHelper x = x

repeatHelper' :: p -> p
repeatHelper' = id

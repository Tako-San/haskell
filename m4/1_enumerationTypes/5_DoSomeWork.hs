module DoSomeWork where

data Result = Fail | Success

doSomeWork :: a -> (Result, Int)
doSomeWork a = (Fail, 0)

processData :: a -> String
processData x = case doSomeWork x of
  (Success, 0) -> "Success"
  (Success, _) -> error "Error code should be equal to zero on success"
  (Fail   , y) -> "Fail: " ++ show y

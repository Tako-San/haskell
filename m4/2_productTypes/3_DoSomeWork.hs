module DoSomeWork where

data Result = Success | Fail

data Result' = Success' | Fail' Int

instance Show Result' where
  show Success'  = "Success"
  show (Fail' n) = "Fail: " ++ show n

doSomeWork :: a -> (Result, Int)
doSomeWork a = (Success, 0)

doSomeWork' :: a -> Result'
doSomeWork' x = case doSomeWork x of
  (Success, _) -> Success'
  (Fail   , y) -> Fail' y

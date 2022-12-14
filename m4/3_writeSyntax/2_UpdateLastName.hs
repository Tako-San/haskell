module UpdateLastName where

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }
  deriving (Read, Show)

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1 }


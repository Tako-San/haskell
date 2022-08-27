module AbbrFirstName where

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }
  deriving (Show, Read)

abbrFirstName :: Person -> Person
abbrFirstName p@Person { firstName = fn }
  | length fn <= 2 = p
  | otherwise      = p { firstName = head fn : "." }

abbrFirstName' :: Person -> Person
abbrFirstName' p@Person { firstName = (c1:c2:tl)} = p { firstName = c1 : "." }
abbrFirstName' p = p

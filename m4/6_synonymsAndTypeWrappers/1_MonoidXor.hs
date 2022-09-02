module MonoidXor where

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
  a <> b = undefined

instance Monoid Xor where
  mempty = Xor False
  mappend a b = Xor $ getXor a /= getXor b

module MonoidMaybe where

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
     (<>) a b = undefined

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty
  mappend x mn@(Maybe' Nothing) = mn
  mappend mn@(Maybe' Nothing) x = mn
  mappend (Maybe' (Just x)) (Maybe' (Just y)) = Maybe' $ Just $ mappend x y


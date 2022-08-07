module KnowTo where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab inp | doesEnrageMork inp && doesEnrageGork inp = stab $ stomp inp
                    | doesEnrageMork inp = stomp inp
                    | doesEnrageGork inp = stab inp
                    | otherwise          = inp

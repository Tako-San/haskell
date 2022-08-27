module AddMul where

data Bit = Zero | One deriving (Show, Read)
data Sign = Minus | Plus deriving (Show, Read)
data Z = Z Sign [Bit]
  deriving (Show, Read)


add :: Z -> Z -> Z
add term1 term2 = toZ $ fromZ term1 + fromZ term2

mul :: Z -> Z -> Z
mul factor1 factor2 = toZ $ fromZ factor1 * fromZ factor2

fromBit :: Bit -> Integer
fromBit Zero = 0
fromBit One  = 1

toBit :: Integer -> Bit
toBit 0 = Zero
toBit 1 = One
toBit _ = error "Number is not equal to 1 or 0"

fromZ :: Z -> Integer
fromZ (Z Minus lst) = -fromZ (Z Plus lst)
fromZ (Z Plus  lst) = helper 0 0 lst where
  helper acc deg []       = acc
  helper acc deg (x : xs) = helper (acc + fromBit x * 2 ^ deg) (deg + 1) xs

toZ :: Integer -> Z
toZ num | num < 0   = Z Minus $ helper [] (-num)
        | otherwise = Z Plus $ helper [] num
 where
  helper []  0 = [Zero]
  helper acc 0 = acc
  helper acc n = helper (acc ++ [toBit (n `mod` 2)]) (n `div` 2)

module Nat where

data Nat = Zero | Suc Nat deriving(Read, Show)

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add x Zero    = x
add x (Suc y) = add (Suc x) y

mul :: Nat -> Nat -> Nat
mul = helper Zero where
  helper acc Zero _          = acc
  helper acc _    Zero       = acc
  helper acc lhs  (Suc sRhs) = helper (acc `add` lhs) lhs sRhs

fac :: Nat -> Nat
fac = helper (Suc Zero) where
  helper acc Zero           = acc
  helper acc val@(Suc sVal) = helper (acc `mul` val) sVal

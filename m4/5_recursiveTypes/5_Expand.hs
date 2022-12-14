module Expand where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e | expand' e == e = e
         | otherwise     = expand $ expand' e

expand' :: Expr -> Expr
expand' ((e1 :+: e2) :*: e) =
  expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
expand' (e :*: (e1 :+: e2)) =
  expand' e :*: expand' e1 :+: expand' e :*: expand' e2
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' e           = e

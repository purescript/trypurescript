module Main where

import Prelude

data Maybe a = Nothing | Just a

instance monadMaybe :: Prelude.Monad Maybe where
  return = Just
  (>>=) Nothing _ = Nothing
  (>>=)  (Just a) f = f a

isEven n | n % 2 == 0 = Just {}
isEven _ = Nothing

evenSum a b = do
  n <- a
  m <- b
  let sum = n + m
  isEven sum
  return sum


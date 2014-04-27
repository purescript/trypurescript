module Main where

data Maybe a = Nothing | Just a

--
-- In order to define an instance of Monad for the Maybe type,
-- we need to provide instaces for its superclasses:
-- Functor, Apply, Applicative and Bind.
--

instance functorMaybe :: Functor Maybe where
  (<$>) _ Nothing = Nothing
  (<$>) f (Just a) = Just a

instance applyMaybe :: Apply Maybe where
  (<*>) = ap

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

instance monadMaybe :: Monad Maybe

--
-- In this example, we find the sum of two numbers, using the guard
-- function to make sure the sum is even.
--

guard :: Boolean -> Maybe {}
guard true = Just {}
guard false = Nothing

evenSum :: Maybe Number -> Maybe Number -> Maybe Number
evenSum a b = do
  n <- a
  m <- b
  let sum = n + m
  guard $ sum % 2 == 0
  return sum


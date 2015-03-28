module Main where

import Debug.Trace

instance functorArray :: Functor [] where
  (<$>) _ [] = []
  (<$>) f (x:xs) = f x : (f <$> xs)

instance applyArray :: Apply [] where
  (<*>) = ap

instance applicativeArray :: Applicative [] where
  pure a = [a]

concat :: forall a. [a] -> [a] -> [a]
concat []     ys = ys
concat (x:xs) ys = x : concat xs ys

instance bindArray :: Bind [] where
  (>>=) [] _ = []
  (>>=) (x:xs) f = concat (f x) (xs >>= f)

instance monadArray :: Monad []

guard :: Boolean -> [Unit]
guard true = [unit]
guard false = []

triples :: [Number] -> [[Number]]
triples ns = do
  x <- ns
  y <- ns
  z <- ns
  guard $ x * x + y * y == z * z
  return [x, y, z]
  
main = print $ triples [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
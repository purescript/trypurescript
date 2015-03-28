module Main where

import Debug.Trace

class Sized a where
  size :: a -> Number

instance sizedNumber :: Sized Number where
  size n = n

instance sizedArray :: (Sized a) => Sized [a] where
  size [] = 0
  size (x : xs) = size x + size xs

main = do
  print $ size 100
  print $ size [1, 2, 3, 4, 5]
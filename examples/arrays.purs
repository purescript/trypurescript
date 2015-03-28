module Main where

import Debug.Trace

sum :: [Number] -> Number
sum (x:xs) = x + sum xs
sum _ = 0

main = do
  let ns = [1, 2, 3, 4, 5]
  print $ sum ns

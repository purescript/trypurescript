module Main where

import Debug.Trace

isOdd :: Number -> Boolean
isOdd 0 = false
isOdd n = isEven (n - 1)

isEven :: Number -> Boolean
isEven 0 = true
isEven n = isOdd (n - 1)

main = do
  print $ isEven 1000
  print $ isEven 1001
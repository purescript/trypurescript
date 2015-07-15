module Main where

import Prelude
import Control.Monad.Eff.Console (print)

isOdd :: Int -> Boolean
isOdd 0 = false
isOdd n = isEven (n - 1)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = isOdd (n - 1)

main = do
  print $ isEven 1000
  print $ isEven 1001
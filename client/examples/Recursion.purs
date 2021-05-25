module Main where

import Prelude
import Effect.Console (logShow)
import TryPureScript (render, withConsole)

isOdd :: Int -> Boolean
isOdd 0 = false
isOdd n = isEven (n - 1)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = isOdd (n - 1)

main = render =<< withConsole do
  logShow $ isEven 1000
  logShow $ isEven 1001

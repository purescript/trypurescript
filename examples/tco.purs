module Main where

import Prelude
import Control.Monad.Eff.Console (print)

fact :: Int -> Int
fact = go 1
  where
  go prod 0 = prod
  go prod n = go (prod * n) (n - 1)

main = print $ fact 10
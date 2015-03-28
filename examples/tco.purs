module Main where

import Debug.Trace

fact :: Number -> Number
fact = go 1
  where
  go prod 0 = prod
  go prod n = go (prod * n) (n - 1)

main = print $ fact 20
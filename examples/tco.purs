module Main where

import Prelude

fact = go 1
  where
  go prod 0 = prod
  go prod n = go (prod * n) (n - 1)


module Main where

import Prelude

factHelper prod 0 = prod
factHelper prod n = factHelper (prod * n) (n - 1)

fact = factHelper 1


module Main where

import Prelude
import Control.MonadPlus (guard)
import Effect.Console (logShow)
import Data.Array ((..))
import Data.Foldable (for_)
import TryPureScript

-- Find Pythagorean triples using an array comprehension.
triples :: Int -> Array (Array Int)
triples n = do
  z <- 1 .. n
  y <- 1 .. z
  x <- 1 .. y
  guard $ x * x + y * y == z * z
  pure [x, y, z]

main = render =<< withConsole do
  for_ (triples 20) logShow

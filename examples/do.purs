module Main where

import Prelude
import Control.Monad.Eff.Console (print)

-- | Find Pythagorean triples using an 
-- | array comprehension.
triples :: Array Int -> Array (Array Int)
triples ns = do
  x <- ns
  y <- ns
  z <- ns
  if (x < y && x * x + y * y == z * z)
    then return [x, y, z]
    else []
  
main = print $ triples [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
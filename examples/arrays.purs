module Main where

import Prelude

sum (x:xs) = x + sum xs
sum _ = 0

sumOfProducts (x : y : xs) = x * y + sumOfProducts xs
sumOfProducts _ = 0

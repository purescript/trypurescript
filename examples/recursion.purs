module Main where

import Prelude

isOdd :: Number -> Boolean
isOdd 0 = false
isOdd n = isEven (n - 1)

isEven :: Number -> Boolean
isEven 0 = true
isEven n = isOdd (n - 1)


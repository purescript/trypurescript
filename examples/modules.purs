module M1 where

import Prelude

incr :: Number -> Number
incr x = x + 1

module Main where

test = M1.incr 10


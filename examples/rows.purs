module Main where

import Prelude
import Control.Monad.Eff.Console (log)

showPerson o = o.lastName ++ ", " ++ o.firstName

main = log $ showPerson 
  { firstName: "John"
  , lastName: "Smith"
  , age: 30
  }

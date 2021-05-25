module Main where

import Prelude
import Effect.Console (log)
import TryPureScript (render, withConsole)

-- We can write functions which require certain record labels...
showPerson o = o.lastName <> ", " <> o.firstName

-- ... but we are free to call those functions with any
-- additional arguments, such as "age" here.
main = render =<< withConsole do
  log $ showPerson
    { firstName: "John"
    , lastName: "Smith"
    , age: 30
    }

module Main where

import Debug.Trace

showPerson o = o.lastName ++ ", " ++ o.firstName

main = trace $ showPerson 
  { firstName: "John"
  , lastName: "Smith"
  , age: "30"
  }

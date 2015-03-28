module Main where

import Control.Monad.Eff

import Debug.Trace

foreign import data Random :: !

foreign import random 
  "function random() {\
  \  return Math.random();\
  \}" :: forall eff. Eff (random :: Random | eff) Number

main = do
  n <- random
  print n

module Main where

import Control.Monad.Eff

foreign import data Random :: !

foreign import random """
  function random() {
    return Math.random();
  }""" :: forall eff. Eff (random :: Random | eff) Number

main = random >>= Debug.Trace.print


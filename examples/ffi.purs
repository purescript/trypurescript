module Main where

import Control.Monad.Eff

foreign import data Random :: !

foreign import random """
  function random() {
    return Math.random();
  }""" :: Eff (random :: Random) Number

main = Debug.Trace.print <$> random


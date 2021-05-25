module Main where

import Prelude

import Effect.Console (log)
import Data.Array ((..))
import Data.Foldable (for_)
import TryPureScript (render, withConsole)

main = render =<< withConsole do
  for_ (10 .. 1) \n -> log (show n <> "...")
  log "Lift off!"

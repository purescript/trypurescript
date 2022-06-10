module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Try.Container as Container

main :: Effect Unit
main = void $ launchAff_ do
  body <- HA.awaitBody
  void $ runUI Container.component unit body

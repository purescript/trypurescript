module Main where

import Prelude
import Data.Array (sort)
import Test.QuickCheck (quickCheck, (===))
import TryPureScript (render, withConsole, h1, h2, p, text)

main = do
  render $ h1 $ text "QuickCheck"
  render $ p $ text """QuickCheck is a Haskell library which allows us to assert properties
hold for our functions. QuickCheck uses type classes to generate
random test cases to verify those properties.
purescript-quickcheck is a port of parts of the QuickCheck library to
PureScript."""
  render $ h2 $ text "Sort function is idempotent"
  render =<< withConsole do
    quickCheck \(xs :: Array Int) -> sort (sort xs) === sort xs
  render $ h2 $ text "Every array is sorted"
  render $ p $ text "This test should fail on some array which is not sorted"
  render =<< withConsole do
    quickCheck \(xs :: Array Int) -> sort xs === xs

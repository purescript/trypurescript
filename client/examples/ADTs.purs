module Main where

import Prelude

import Effect.Console (logShow)
import Data.Map (Map, lookup, singleton)
import TryPureScript (render, withConsole)

-- | A Name consists of a first name and a last name
data Name = Name String String

-- | With compiler versions >= 0.8.2, we can derive
-- | instances for Eq and Ord, making names comparable.
derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

-- | The Ord instance allows us to use Names as the
-- | keys in a Map.
phoneBook :: Map Name String
phoneBook = singleton (Name "John" "Smith") "555-555-1234"

main = render =<< withConsole do
  logShow (lookup (Name "John" "Smith") phoneBook)

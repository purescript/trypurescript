module JQuery.Extras
  ( click
  , empty
  , fadeIn
  , fadeOut
  , filter
  , is
  , getValueMaybe
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import JQuery (JQuery, Selector)

-- | Simulate a click event on the specified element.
foreign import click :: JQuery -> Effect Unit

-- | Remove all elements from the specified container element.
foreign import empty :: JQuery -> Effect Unit

-- | Fade in an element.
foreign import fadeIn :: JQuery -> Effect Unit

-- | Fade out an element.
foreign import fadeOut :: JQuery -> Effect Unit

-- | Filter elements based on an additional selector.
foreign import filter :: JQuery -> Selector -> Effect JQuery

-- | Test whether elements match an additional selector.
foreign import is :: JQuery -> Selector -> Effect Boolean

-- | Get the value of the first element, if it exists.
foreign import getValue :: JQuery -> Effect (Nullable String)

getValueMaybe :: JQuery -> Effect (Maybe String)
getValueMaybe = map toMaybe <<< getValue

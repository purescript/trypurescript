module Control.Monad.Eff.JQuery.Extras
  ( click
  , empty
  , fadeIn
  , fadeOut
  , filter
  , is
  , getValueMaybe
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery, Selector)
import DOM (DOM)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

-- | Simulate a click event on the specified element.
foreign import click :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit

-- | Remove all elements from the specified container element.
foreign import empty :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit

-- | Fade in an element.
foreign import fadeIn :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit

-- | Fade out an element.
foreign import fadeOut :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit

-- | Filter elements based on an additional selector.
foreign import filter :: forall eff. JQuery -> Selector -> Eff (dom :: DOM | eff) JQuery

-- | Test whether elements match an additional selector.
foreign import is :: forall eff. JQuery -> Selector -> Eff (dom :: DOM | eff) Boolean

-- | Get the value of the first element, if it exists.
foreign import getValue :: forall eff. JQuery -> Eff (dom :: DOM | eff) (Nullable String)

getValueMaybe :: forall eff. JQuery -> Eff (dom :: DOM | eff) (Maybe String)
getValueMaybe = map toMaybe <<< getValue

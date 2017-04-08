module Thermite.Try where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Partial.Unsafe (unsafePartial)
import React (createFactory) as R
import ReactDOM (render) as R
import Thermite as T

-- | The main method creates the task list component, and renders it to the document body.
defaultMain :: forall state action eff. T.Spec eff state Unit action -> state -> Eff (dom :: DOM.DOM | eff) Unit
defaultMain spec initialState = void do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial fromJust <$> DOM.querySelector (wrap "#app") (DOM.htmlDocumentToParentNode document)
  R.render (R.createFactory component unit) container

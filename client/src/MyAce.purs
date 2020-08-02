module MyAce where

import Ace (EditSession, Marker)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)

{-
Fixes some issues in package.
Todo - make PRs for these.
-}
-- Returns array of marker IDs
-- Boolean to indicate front or back
foreign import getMarkersImpl :: Fn2 Boolean EditSession (Effect (Array Marker))

getMarkers :: Boolean -> EditSession -> Effect (Array Marker)
getMarkers inFront session = runFn2 getMarkersImpl inFront session

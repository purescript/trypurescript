module FRP.Try (defaultMain) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import FRP (FRP)
import FRP.Behavior (Behavior, animate)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, render)

defaultMain :: Behavior Drawing -> Eff (canvas :: CANVAS, frp :: FRP) Unit
defaultMain b = do
  canvas <- getCanvasElementById "canvas"
  for_ canvas \c -> do
    ctx <- getContext2D c
    animate b (render ctx)

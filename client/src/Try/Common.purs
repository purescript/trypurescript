module Try.Common where

import Prelude

{-
Common values and newtype wrappers
-}
--
-- Page to launch on startup and when clicking home button
homeRoute :: String
homeRoute = "/?gist=93b9116a309c97af2af5d3f06f6a4479"

-- Query param for compressed code.
pursQP :: String
pursQP = "purs"

-- Query param for gist.
gistQP :: String
gistQP = "gist"

newtype Compressed
  = Compressed String

instance showCompressed :: Show Compressed where
  show (Compressed c) = c

newtype Content
  = Content String

instance showContent :: Show Content where
  show (Content c) = c

derive instance eqContent :: Eq Content

newtype GistID
  = GistID String

instance showGistID :: Show GistID where
  show (GistID g) = g

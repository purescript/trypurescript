module TPS.Common where

import Prelude

{-
Common values and newtype wrappers
-}
--
-- Page to launch on startup and when clicking home button
homeRoute :: String
homeRoute = "/?github=milesfrain/tps/demo/examples/Home.purs"

-- Query param for compressed code.
pursQP :: String
pursQP = "purs"

-- Query param for gist.
gistQP :: String
gistQP = "gist"

-- Query param for path to file on github
ghPathQP :: String
ghPathQP = "github"

newtype AuthCode
  = AuthCode String

instance showAuthCode :: Show AuthCode where
  show (AuthCode c) = c

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

newtype GhToken
  = GhToken String

instance showToken :: Show GhToken where
  show (GhToken t) = t

newtype GhPath
  = GhPath String

instance showGhPath :: Show GhPath where
  show (GhPath p) = p

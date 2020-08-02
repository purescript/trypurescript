module TPS.Utility where

import Prelude
import TPS.Common (Compressed(..), Content(..), GistID, pursQP)
import TPS.Config (appDomain, clientID)
import Effect (Effect)
import LzString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

{-
Helper functions that can exist outside of the main component.
-}
--
data ViewMode
  = SideBySide
  | Code
  | Output

-- Could alternatively derive if displaying "SideBySide"(no hyphens) is okay.
instance showViewMode :: Show ViewMode where
  show SideBySide = "Side-by-side"
  show Code = "Code"
  show Output = "Output"

derive instance eqViewMode :: Eq ViewMode

type PushRoute
  = String -> Effect Unit

data ContentSource
  = NewContent --NoGist
  | SavingGist
  | HaveGist GistID
  | HaveGhFile String

compress :: Content -> Compressed
compress (Content c) = Compressed $ compressToEncodedURIComponent c

decompress :: Compressed -> Content
decompress (Compressed c) = Content $ decompressFromEncodedURIComponent c

ghAuthorize :: Content -> Effect Unit
ghAuthorize content = do
  win <- window
  loc <- location win
  -- I believe it's fine for client ID to be public information
  let
    authUrl =
      "https://github.com/login/oauth/authorize?"
        <> "client_id="
        <> clientID
        <> "&scope=gist"
        <> "&redirect_uri="
        <> appDomain
        <> "/?"
        <> pursQP
        <> "="
        <> (show $ compress content)
  setHref authUrl loc

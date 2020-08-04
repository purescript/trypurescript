module Try.Utility where

import Prelude
import Try.Common (Compressed(..), Content(..), GistID)
import Effect (Effect)
import LzString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)

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

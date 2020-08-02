module Try.Gist where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (decodeJson, stringify)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Try.Common (Content(..), GistID(..))

{-
Handles HTTP requests for fetching github gists.
-}
--
gistApiUrl :: String
gistApiUrl = "https://api.github.com/gists"

type GistJson
  = { files :: { "Main.purs" :: { content :: String } }
    }

getGistContent :: GistJson -> Content
getGistContent obj = Content obj.files."Main.purs".content

ghGetGist :: GistID -> Aff (Either String Content)
ghGetGist (GistID gistID) = do
  result <- AX.get AXRF.json $ gistApiUrl <> "/" <> gistID
  pure
    $ case result of
        Left err -> Left $ "GET gist response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: GistJson) -> Right $ getGistContent decoded

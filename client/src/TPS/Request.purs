module TPS.Request where

import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (decodeJson, encodeJson)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import TPS.Common (AuthCode(..), Content(..), GhToken(..), GistID(..))
import TPS.Config (tokenServerUrl)

{-
Handles HTTP requests for fetching github files and gists,
and saving gists.
-}
--
type TokenResp
  = { access_token :: String }

ghRequestToken :: AuthCode -> Aff (Either String GhToken)
ghRequestToken (AuthCode code) = do
  result <- AX.post AXRF.json tokenServerUrl $ Just $ AXRB.json $ encodeJson { code }
  pure
    $ case result of
        Left err -> do
          Left $ "POST /api response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: TokenResp) -> Right $ GhToken decoded.access_token

gistApiUrl :: String
gistApiUrl = "https://api.github.com/gists"

type GistJson
  = { files :: { "Main.purs" :: { content :: String } }
    }

type GistJsonWithDescription
  = { files ::
        { "Main.purs" ::
            { content :: String
            , description :: String
            }
        }
    }

setGistContent :: Content -> GistJsonWithDescription
setGistContent (Content content) =
  { files:
      { "Main.purs":
          { content
          , description: "Created by TryPurescript"
          }
      }
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
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: GistJson) -> Right $ getGistContent decoded

getFile :: String -> Aff (Either String Content)
getFile url = do
  result <- AX.get AXRF.string url
  pure
    $ case result of
        Left err -> Left $ "Failed to get file at: " <> url <> ", " <> AX.printError err
        Right response -> Right $ Content response.body

ghCreateGist :: GhToken -> Content -> Aff (Either String GistID)
ghCreateGist token content = do
  result <-
    AX.request
      ( AX.defaultRequest
          { url = gistApiUrl
          , method = Left POST
          , responseFormat = AXRF.json
          , headers = [ AXRH.RequestHeader "Authorization" $ "token " <> show token ]
          , content = Just $ AXRB.json $ encodeJson $ setGistContent content
          }
      )
  pure
    $ case result of
        Left err -> do
          Left $ "POST /api response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: { id :: String }) -> Right $ GistID decoded.id

module Try.Gist
  ( GistInfo
  , uploadGist
  , getGistById
  , tryLoadFileFromGist
  ) where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Argonaut.Core (Json, caseJsonObject, stringify, toString)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)

-- | An abstract data type representing the data we get back from the GitHub API.
data GistInfo

foreign import rawUrl_ :: Fn2 GistInfo String (Nullable String)

-- | Retrieve the URL for the raw contents of a particular file within a gist,
-- | if that file exists as part of the gist.
rawUrl :: GistInfo -> String -> Either String String
rawUrl gist filename = case toMaybe $ runFn2 rawUrl_ gist filename of
  Nothing ->
    Left $ "Gist does not contain a file named " <> filename
  Just url ->
    Right url

uploadGist :: String -> ExceptT String Aff String
uploadGist content = ExceptT $ AX.post AXRF.json "https://api.github.com/gists" requestBody >>= case _ of
  Left e ->
    pure $ Left $ "Unable to load Gist metadata: \n" <> printError e
  Right { status } | status >= StatusCode 400 ->
    pure $ Left $ "Received error status code: " <> show status
  Right { body } ->
    pure $ body # caseJsonObject (Left $ "Expected object in uploadGist, received: " <> stringify body) \obj ->
      case Object.lookup "id" obj of
        Just v | Just v' <- toString v -> Right v'
        Nothing -> Left "No id key found."
        _ -> Left "Key id was not a string."

  where
  requestBody = Just $ AXRB.json $ encodeJson
    { description: "Published with try.purescript.org"
    , public: false
    , files: { "Main.purs": { content } }
    }

getGistById :: String -> ExceptT String Aff GistInfo
getGistById id = ExceptT $ AX.get AXRF.json ("https://api.github.com/gists/" <> id) >>= case _ of
  Left e ->
    pure $ Left $ "Unable to load Gist metadata: \n" <> printError e
  Right { status } | status >= StatusCode 400 ->
    pure $ Left $ "Received error status code: " <> show status
  Right { body } ->
    pure $ Right $ (unsafeCoerce :: Json -> GistInfo) body

tryLoadFileFromGist :: GistInfo -> String -> ExceptT String Aff String
tryLoadFileFromGist gi filename = do
  url <- ExceptT $ pure $ rawUrl gi filename
  ExceptT $ AX.get AXRF.string url >>= case _ of
    Left e ->
      pure $ Left $ "Unable to load gist contents: \n" <> printError e
    Right { status } | status >= StatusCode 400 ->
      pure $ Left $ "Received error status code: " <> show status
    Right { body } ->
      pure $ Right body

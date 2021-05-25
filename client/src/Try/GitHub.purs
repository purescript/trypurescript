module Try.GitHub where

import Prelude

import Affjax (URL, printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either(..))
import Effect.Aff (Aff)

mkGitHubUrl :: String -> URL
mkGitHubUrl id = "https://raw.githubusercontent.com/" <> id

getRawGitHubFile :: String -> ExceptT String Aff String
getRawGitHubFile id = ExceptT $ AX.get AXRF.string (mkGitHubUrl id) >>= case _ of
  Left e ->
    pure $ Left $ "Unable to load file from GitHub: \n" <> printError e
  Right { status } | status >= StatusCode 400 ->
    pure $ Left $ "Received error status code: " <> show status
  Right { body } ->
    pure $ Right body

module Try.API
  ( ErrorPosition(..)
  , CompilerError(..)
  , CompileError(..)
  , CompileWarning(..)
  , Suggestion(..)
  , SuccessResult(..)
  , FailedResult(..)
  , CompileResult(..)
  , get
  , compile
  ) where

import Prelude

import Affjax (URL, printError)
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

-- | The range of text associated with an error
type ErrorPosition =
  { startLine :: Int
  , endLine :: Int
  , startColumn :: Int
  , endColumn :: Int
  }

type CompilerError =
  { message :: String
  , position :: Maybe ErrorPosition
  }

-- | An error reported from the compile API.
data CompileError
  = CompilerErrors (Array CompilerError)
  | OtherError String

instance decodeJsonCompileError :: DecodeJson CompileError where
  decodeJson = decodeJson >=> \obj -> do
    contents <- obj .: "contents"
    obj .: "tag" >>= case _ of
      "OtherError" ->
        map OtherError $ decodeJson contents
      "CompilerErrors" ->
        map CompilerErrors $ traverse decodeJson =<< decodeJson contents
      _ ->
        Left "Tag must be one of: OtherError, CompilerErrors"

type Suggestion =
  { replacement :: String
  , replaceRange :: Maybe ErrorPosition
  }

type CompileWarning =
  { errorCode :: String
  , message :: String
  , position :: Maybe ErrorPosition
  , suggestion :: Maybe Suggestion
  }

type SuccessResult =
  { js :: String
  , warnings :: Maybe (Array CompileWarning)
  }

type FailedResult =
  { error :: CompileError
  }

-- | The result of calling the compile API.
data CompileResult
  = CompileSuccess SuccessResult
  | CompileFailed FailedResult

-- | Parse the result from the compile API and verify it
instance decodeJsonCompileResult :: DecodeJson CompileResult where
  decodeJson json =
    map CompileSuccess (decodeJson json)
      <|> map CompileFailed (decodeJson json)

get :: URL -> ExceptT String Aff String
get url = ExceptT $ AX.get AXRF.string url >>= case _ of
  Left e ->
    pure $ Left $ printError e
  Right { status } | status >= StatusCode 400 ->
    pure $ Left $ "Received error status code: " <> show status
  Right { body } ->
    pure $ Right body

-- | POST the specified code to the Try PureScript API, and wait for a response.
compile :: forall m. MonadAff m => String -> String -> ExceptT String m (Either String CompileResult)
compile endpoint code = ExceptT $ liftAff $ AX.post AXRF.json (endpoint <> "/compile") requestBody >>= case _ of
  Left e ->
    pure $ Left $ printError e
  Right { status } | status >= StatusCode 400 ->
    pure $ Left $ "Received error status code: " <> show status
  Right { body } ->
    pure $ Right $ decodeJson body
  where
  requestBody = Just $ AXRB.string code

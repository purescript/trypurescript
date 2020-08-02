module TPS.Compile where

import Prelude
import Ace (Range)
import Ace.Range as Range
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Argonaut.Core as J
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJsonWith)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import TPS.Common (Content(..))
import TPS.Config (compileUrl)

------- Compile API types -------
--
-- The result of calling the compile API.
data CompileResult
  = CompileSuccess SuccessResult
  | CompileFailed FailedResult

-- A successful compilation result
type SuccessResult
  = { js :: String
    , warnings :: Maybe (Array CompileWarning)
    }

-- A warning about the code found during compilation
type CompileWarning
  = { errorCode :: String
    , message :: String
    , position :: Maybe ErrorPosition
    , suggestion :: Maybe Suggestion
    }

-- The range of text associated with an error or warning
type ErrorPosition
  = { startLine :: Int
    , endLine :: Int
    , startColumn :: Int
    , endColumn :: Int
    }

-- A code suggestion
type Suggestion
  = { replacement :: String
    , replaceRange :: Maybe ErrorPosition
    }

-- A failed compilation result
type FailedResult
  = { error :: CompileError }

-- An error reported from the compile API
data CompileError
  = CompilerErrors (Array CompilerError)
  -- Examples of `OtherError` include:
  -- * Code is not "module Main"
  -- * The code snippet is too large
  | OtherError String

-- An error found with the code during compilation
type CompilerError
  = { message :: String
    , position :: Maybe ErrorPosition
    }

------- Json Decoding -------
--
-- The Compile API returns an object representing the contents of either:
-- * CompileSuccess
-- * CompileFailed
-- Decoding to CompileResult requires attempting to match each of these.
instance decodeJsonCompileResult :: DecodeJson CompileResult where
  decodeJson j =
    CompileSuccess <$> decodeJson j
      <|> CompileFailed
      <$> decodeJson j

derive instance genericCompileResult :: Generic CompileResult _

-- The Compile API encodes the CompileError tagged union differently than
-- argonaut's generic options, so we need to adjust the default encoding
-- options to successfully decode.
instance decodeJsonCompileError :: DecodeJson CompileError where
  decodeJson =
    genericDecodeJsonWith
      $ defaultEncoding
          { valuesKey = "contents"
          , unwrapSingleArguments = true
          }

derive instance genericCompileError :: Generic CompileError _

-- temp
instance showCompileResult :: Show CompileResult where
  show = genericShow

instance showCompileError :: Show CompileError where
  show = genericShow

-- | POST the specified code to the Try PureScript API, and wait for
-- | a response.
compile :: Content -> Aff (Either String CompileResult)
compile (Content ct) = do
  result <- AX.post AXRF.json (compileUrl <> "/compile") $ Just $ AXRB.string ct
  pure
    $ case result of
        Left err -> Left $ "POST compile response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: CompileResult) -> Right decoded

------  generate errors for editor --------------
-- Todo - move this to another file
type Annotation
  = { row :: Int
    , column :: Int
    , type :: String
    , text :: String
    }

-- | Set the gutter annotations
--foreign import setAnnotations :: EffectFn1 (Array Annotation) Unit
data AnnotationType
  = AnnotateWarning
  | AnnotateError

instance showAnnotationType :: Show AnnotationType where
  show AnnotateWarning = "warning"
  show AnnotateError = "error"

-- Common fields of CompileWarning and CompilerError
-- Todo - should both of these have `er` ending?
type WarningOrError r
  = { message :: String
    , position :: Maybe ErrorPosition
    | r
    }

-- Creates an annotation from a warning or error,
-- but only if there's a position.
toAnnotation :: forall r. AnnotationType -> WarningOrError r -> Maybe Annotation
toAnnotation _ { position: Nothing } = Nothing

toAnnotation annType { position: Just pos, message } =
  Just
    { row: pos.startLine - 1
    , column: pos.startColumn - 1
    , type: show annType
    , text: message
    }

-- Make sure position's range is at least one character wide.
nonZeroRange :: ErrorPosition -> ErrorPosition
nonZeroRange p =
  if p.startLine == p.endLine && p.endColumn <= p.startColumn then
    if p.startColumn > 0 then
      p { startColumn = p.endColumn - 1 }
    else
      p { endColumn = p.startColumn + 1 }
  else
    p

-- Creates a Range for making Markers from a warning or error,
-- but only if there's a position.
mkMarkerRange :: forall r. WarningOrError r -> Effect (Maybe Range)
mkMarkerRange { position: Nothing } = pure Nothing

mkMarkerRange { position: Just p0 } = do
  let
    p = nonZeroRange p0
  rg <-
    Range.create
      (p.startLine - 1)
      (p.startColumn - 1)
      (p.endLine - 1)
      (p.endColumn - 1)
  pure $ Just rg

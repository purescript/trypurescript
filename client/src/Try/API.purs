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

import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (ContT(ContT))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)
import Foreign (Foreign, ForeignError)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Generic.Class (Options, SumEncoding(..))

decodingOptions :: Options
decodingOptions = defaultOptions { unwrapSingleConstructors = true }

-- | The range of text associated with an error
newtype ErrorPosition = ErrorPosition
  { startLine :: Int
  , endLine :: Int
  , startColumn :: Int
  , endColumn :: Int
  }

derive instance genericErrorPosition :: Generic ErrorPosition _

instance decodeErrorPosition :: Decode ErrorPosition where
  decode = genericDecode decodingOptions

newtype CompilerError = CompilerError
  { message :: String
  , position :: Maybe ErrorPosition
  }

derive instance genericCompilerError :: Generic CompilerError _

instance decodeCompilerError :: Decode CompilerError where
  decode = genericDecode decodingOptions

-- | An error reported from the compile API.
data CompileError
  = CompilerErrors (Array CompilerError)
  | OtherError String

derive instance genericCompileError :: Generic CompileError _

instance decodeCompileError :: Decode CompileError where
  decode = genericDecode
    (defaultOptions
      { sumEncoding =
          TaggedObject
            { tagFieldName: "tag"
            , contentsFieldName: "contents"
            , constructorTagTransform: identity
            }
      })

newtype Suggestion = Suggestion
  { replacement :: String
  , replaceRange :: Maybe ErrorPosition
  }

derive instance genericSuggestion :: Generic Suggestion _

instance decodeSuggestion :: Decode Suggestion where
  decode = genericDecode decodingOptions

newtype CompileWarning = CompileWarning
  { errorCode :: String
  , message :: String
  , position :: Maybe ErrorPosition
  , suggestion :: Maybe Suggestion
  }

derive instance genericCompileWarning :: Generic CompileWarning _

instance decodeCompileWarning :: Decode CompileWarning where
  decode = genericDecode decodingOptions

newtype SuccessResult = SuccessResult
  { js :: String
  , warnings :: Maybe (Array CompileWarning)
  }

derive instance genericSuccessResult :: Generic SuccessResult _

instance decodeSuccessResult :: Decode SuccessResult where
  decode = genericDecode decodingOptions

newtype FailedResult = FailedResult
  { error :: CompileError }

derive instance genericFailedResult :: Generic FailedResult _

instance decodeFailedResult :: Decode FailedResult where
  decode = genericDecode decodingOptions

-- | The result of calling the compile API.
data CompileResult
  = CompileSuccess SuccessResult
  | CompileFailed FailedResult

-- | Parse the result from the compile API and verify it
instance decodeCompileResult :: Decode CompileResult where
  decode f =
    CompileSuccess <$> genericDecode decodingOptions f
    <|> CompileFailed <$> genericDecode decodingOptions f

foreign import get_
  :: EffectFn3
            String
            (EffectFn1 String Unit)
            (EffectFn1 String Unit)
            Unit

-- | A wrapper for `get` which uses `ContT`.
get :: String -> ExceptT String (ContT Unit Effect) String
get uri = ExceptT (ContT \k -> runEffectFn3 get_ uri (mkEffectFn1 (k <<< Right)) (mkEffectFn1 (k <<< Left)))

-- | POST the specified code to the Try PureScript API, and wait for
-- | a response.
foreign import compile_
  :: EffectFn4
            String
            String
            (EffectFn1 Foreign Unit)
            (EffectFn1 String Unit)
            Unit

-- | A wrapper for `compileApi` which uses `ContT`.
compile
  :: String
  -> String
  -> ExceptT String (ContT Unit Effect)
       (Either (NonEmptyList ForeignError) CompileResult)
compile endpoint code = ExceptT (ContT \k -> runEffectFn4 compile_ endpoint code (mkEffectFn1 (k <<< Right <<< runExcept <<< decode)) (mkEffectFn1 (k <<< Left)))

module Try.API
  ( ErrorPosition(..)
  , CompilerError(..)
  , CompileError(..)
  , SuccessResult(..)
  , FailedResult(..)
  , CompileResult(..)
  , compile
  , get
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, EffFn4, mkEffFn1, runEffFn3, runEffFn4)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT(..))
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Try.Types (BackendConfig)

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
  , position :: ErrorPosition
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
            , constructorTagTransform: id
            }
      })

newtype SuccessResult = SuccessResult
  { js :: String }

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
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `get` which uses `ContT`.
get :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
get uri = ExceptT (ContT \k -> runEffFn3 get_ uri (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

-- | POST the specified code to the Try PureScript API, and wait for
-- | a response.
foreign import compile_
  :: forall eff
   . EffFn4 (dom :: DOM | eff)
            BackendConfig
            String
            (EffFn1 (dom :: DOM | eff) Foreign Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `compileApi` which uses `ContT`.
compile
  :: forall eff
   . BackendConfig
  -> String
  -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff)))
       (Either (NonEmptyList ForeignError) CompileResult)
compile bc code = ExceptT (ContT \k -> runEffFn4 compile_ bc code (mkEffFn1 (k <<< Right <<< runExcept <<< decode)) (mkEffFn1 (k <<< Left)))

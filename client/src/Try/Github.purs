module Try.Github where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)

-- | Fetch a raw file from a GitHub repo via raw.githubusercontent.com
foreign import getRawGithubFile_
  :: EffectFn3 String
               (EffectFn1 String Unit)
               (EffectFn1 String Unit)
               Unit

-- | A wrapper for `getRawGithubFile_` which uses `ContT`.
getRawGithubFile :: String -> ExceptT String (ContT Unit Effect) String
getRawGithubFile id_ = ExceptT (ContT \k -> runEffectFn3 getRawGithubFile_ id_ (mkEffectFn1 (k <<< Right)) (mkEffectFn1 (k <<< Left)))

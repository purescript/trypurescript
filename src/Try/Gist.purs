module Try.Gist
  ( GistInfo
  , uploadGist
  , getGistById
  , tryLoadFileFromGist
  ) where

-- | An abstract data type representing the data we get back from the GitHub API.
import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)

-- | An abstract data type representing the data we get back from the GitHub API.
data GistInfo

foreign import uploadGist_
  :: EffectFn3 String
               (EffectFn1 String Unit)
               (EffectFn1 String Unit)
               Unit

-- | A wrapper for `uploadGist` which uses `ContT`.
uploadGist :: String -> ExceptT String (ContT Unit Effect) String
uploadGist content = ExceptT (ContT \k -> runEffectFn3 uploadGist_ content (mkEffectFn1 (k <<< Right)) (mkEffectFn1 (k <<< Left)))

-- | Get a gist by its ID
foreign import getGistById_
  :: EffectFn3 String
               (EffectFn1 GistInfo Unit)
               (EffectFn1 String Unit)
               Unit

-- | A wrapper for `getGistById` which uses `ContT`.
getGistById :: String -> ExceptT String (ContT Unit Effect) GistInfo
getGistById id_ = ExceptT (ContT \k -> runEffectFn3 getGistById_ id_ (mkEffectFn1 (k <<< Right)) (mkEffectFn1 (k <<< Left)))

foreign import tryLoadFileFromGist_
  :: EffectFn4 GistInfo
               String
               (EffectFn1 String Unit)
               (EffectFn1 String Unit)
               Unit

tryLoadFileFromGist :: GistInfo -> String -> ExceptT String (ContT Unit Effect) String
tryLoadFileFromGist gi filename = ExceptT (ContT \k -> runEffectFn4 tryLoadFileFromGist_ gi filename (mkEffectFn1 (k <<< Right)) (mkEffectFn1 (k <<< Left)))

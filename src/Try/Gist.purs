module Try.Gist
  ( GistInfo
  , uploadGist
  , getGistById
  , tryLoadFileFromGist
  ) where

-- | An abstract data type representing the data we get back from the GitHub API.
import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, EffFn4, mkEffFn1, runEffFn3, runEffFn4)
import Control.Monad.Except.Trans (ExceptT(..))
import DOM (DOM)
import Data.Either (Either(..))

-- | An abstract data type representing the data we get back from the GitHub API.
data GistInfo

foreign import uploadGist_
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `uploadGist` which uses `ContT`.
uploadGist :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
uploadGist content = ExceptT (ContT \k -> runEffFn3 uploadGist_ content (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

-- | Get a gist by its ID
foreign import getGistById_
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) GistInfo Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `getGistById` which uses `ContT`.
getGistById :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) GistInfo
getGistById id_ = ExceptT (ContT \k -> runEffFn3 getGistById_ id_ (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

foreign import tryLoadFileFromGist_
  :: forall eff
   . EffFn4 (dom :: DOM | eff)
            GistInfo
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

tryLoadFileFromGist :: forall eff. GistInfo -> String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
tryLoadFileFromGist gi filename = ExceptT (ContT \k -> runEffFn4 tryLoadFileFromGist_ gi filename (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

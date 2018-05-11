
module Try.Plaste
  ( uploadPlaste
  , getPlasteById
  ) where

-- | An abstract data type representing the data we get back from the Plaste API.
import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, mkEffFn1, runEffFn3)
import Control.Monad.Except.Trans (ExceptT(..))
import DOM (DOM)
import Data.Either (Either(..))

foreign import uploadPlaste_
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `uploadPlaste` which uses `ContT`.
uploadPlaste :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
uploadPlaste content = ExceptT (ContT \k -> runEffFn3 uploadPlaste_ content (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

-- | Get a plaste by its ID
foreign import getPlasteById_
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `getPlasteById` which uses `ContT`.
getPlasteById :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
getPlasteById id_ = ExceptT (ContT \k -> runEffFn3 getPlasteById_ id_ (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))


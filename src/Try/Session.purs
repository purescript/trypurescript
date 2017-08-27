module Try.Session
  ( storeSession
  , tryRetrieveSession
  , createSessionIdIfNecessary
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import DOM (DOM)
import Data.Functor.App (App(..))
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Try.QueryString (getQueryStringMaybe, setQueryString)

randomGuid :: forall eff. Eff (random :: RANDOM | eff) String
randomGuid =
    unwrap (App s4 <> App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> App s4 <> App s4)
  where
    s4 = padLeft <<< toStringAs hexadecimal <$> randomInt 0 (256 * 256)
    padLeft s = String.drop (String.length s - 1) ("000" <> s)

foreign import storeSession_
  :: forall eff
   . EffFn2 (dom :: DOM | eff)
            String
            { code :: String, backend :: String }
            Unit

-- | Store the current session state in local storage
storeSession
  :: forall eff
   . String
  -> { code :: String, backend :: String }
  -> Eff (dom :: DOM | eff) Unit
storeSession sessionId values = runEffFn2 storeSession_ sessionId values

foreign import tryRetrieveSession_
  :: forall eff
   . EffFn1 (dom :: DOM | eff)
            String
            (Nullable { code :: String, backend :: String })

-- | Retrieve the session state from local storage
tryRetrieveSession :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe { code :: String, backend :: String })
tryRetrieveSession sessionId = toMaybe <$> runEffFn1 tryRetrieveSession_ sessionId

-- | Look up the session by ID, or create a new session ID.
createSessionIdIfNecessary
  :: forall eff
   . (String -> Eff (dom :: DOM, random :: RANDOM | eff) Unit)
  -> Eff (dom :: DOM, random :: RANDOM | eff) Unit
createSessionIdIfNecessary k = do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> k sessionId_
    Nothing -> randomGuid >>= setQueryString "session"

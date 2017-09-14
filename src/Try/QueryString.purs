module Try.QueryString
  ( getQueryParams
  , getQueryStringMaybe
  , setQueryString
  , setQueryStrings
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import DOM (DOM)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.StrMap as StrMap
import Data.String as String
import Data.Tuple (Tuple(..))
import Global (decodeURIComponent)

foreign import getQueryString :: forall eff. Eff (dom :: DOM | eff) String

-- | Get all of the URL's query parameters.
getQueryParams :: forall eff. Eff (dom :: DOM | eff) (StrMap.StrMap String)
getQueryParams = breakQueryString <$> getQueryString where
  breakQueryString :: String -> StrMap.StrMap String
  breakQueryString =
    String.drop 1
    >>> String.split (wrap "&")
    >>> map (String.split (wrap "=") >>> parseQueryTerm)
    >>> Array.catMaybes
    >>> StrMap.fromFoldable

  parseQueryTerm :: Array String -> Maybe (Tuple String String)
  parseQueryTerm [k, v] = Just (Tuple k (decodeURIComponent (spaces v)))
  parseQueryTerm _ = Nothing

  spaces :: String -> String
  spaces = String.replaceAll (wrap "+") (wrap " ")

-- | Try to get a key from the URL's query parameters.
getQueryStringMaybe :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe String)
getQueryStringMaybe key = StrMap.lookup key <$> getQueryParams

-- | Set the value of a query string parameter
foreign import setQueryParameters :: forall eff. EffFn1 (dom :: DOM | eff) (StrMap.StrMap String) Unit

-- | Update the specified key in the URL's query parameters.
setQueryString :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
setQueryString k v = setQueryStrings (StrMap.singleton k v)

setQueryStrings :: forall eff. StrMap.StrMap String -> Eff (dom :: DOM | eff) Unit
setQueryStrings ss = do
  params <- getQueryParams
  runEffFn1 setQueryParameters (StrMap.union ss params)

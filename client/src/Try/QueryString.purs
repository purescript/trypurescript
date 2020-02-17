module Try.QueryString
  ( getQueryParams
  , getQueryStringMaybe
  , setQueryString
  , setQueryStrings
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign.Object as Object
import Global.Unsafe (unsafeDecodeURIComponent)

foreign import getQueryString :: Effect String

-- | Get all of the URL's query parameters.
getQueryParams :: Effect (Object.Object String)
getQueryParams = breakQueryString <$> getQueryString where
  breakQueryString :: String -> Object.Object String
  breakQueryString =
    String.drop 1
    >>> String.split (wrap "&")
    >>> map (String.split (wrap "=") >>> parseQueryTerm)
    >>> Array.catMaybes
    >>> Object.fromFoldable

  parseQueryTerm :: Array String -> Maybe (Tuple String String)
  parseQueryTerm [k, v] = Just (Tuple k (unsafeDecodeURIComponent (spaces v)))
  parseQueryTerm _ = Nothing

  spaces :: String -> String
  spaces = String.replaceAll (wrap "+") (wrap " ")

-- | Try to get a key from the URL's query parameters.
getQueryStringMaybe :: String -> Effect (Maybe String)
getQueryStringMaybe key = Object.lookup key <$> getQueryParams

-- | Set the value of a query string parameter
foreign import setQueryParameters :: EffectFn1 (Object.Object String) Unit

-- | Update the specified key in the URL's query parameters.
setQueryString :: String -> String -> Effect Unit
setQueryString k v = setQueryStrings (Object.singleton k v)

setQueryStrings :: Object.Object String -> Effect Unit
setQueryStrings ss = do
  params <- getQueryParams
  runEffectFn1 setQueryParameters (Object.union ss params)

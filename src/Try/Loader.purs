module Try.Loader
  ( Loader
  , makeLoader
  , runLoader
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Try.API as API
import Try.Shim (resolveShims, shims)
import Try.Types (JS(..))

type Module =
  { name :: String
  , path :: Maybe String
  , deps :: Array Dependency
  , src :: JS
  }

type Dependency =
  { name :: String
  , path :: Maybe String
  }

requireRegex :: Regex
requireRegex = unsafeRegex """^var\s+\S+\s*=\s*require\(["']([^"']*)["']\)""" noFlags

dirname :: String -> String
dirname path = fromMaybe "" do
  ix <- String.lastIndexOf (Pattern "/") path
  pure $ String.take ix path

resolvePath :: String -> String -> Maybe String
resolvePath a b
  | String.take 2 b == "./"  = Just $ dirname a <> String.drop 1 b
  | String.take 3 b == "../" = Just $ dirname (dirname a) <> String.drop 2 b
  | otherwise = Nothing

parseDeps :: String -> JS -> Array Dependency
parseDeps current = Array.mapMaybe go <<< String.split (Pattern "\n") <<< unwrap
  where
  go :: String -> Maybe Dependency
  go line = do
    match <- Regex.match requireRegex line
    requirePath <- join $ NonEmpty.index match 1
    pure $ case resolvePath current requirePath of
      Just path ->
        { name: path
        , path: String.stripPrefix (Pattern "/") path
        }
      _ ->
        { name: requirePath
        , path: Nothing
        }

type LoaderResult =
  { ffiDeps :: Array String
  , modules :: Object JS
  }

newtype Loader = Loader (JS -> ExceptT String (ContT Unit Effect) LoaderResult)

runLoader :: Loader -> JS -> ExceptT String (ContT Unit Effect) LoaderResult
runLoader (Loader k) = k

makeLoader :: String -> Loader
makeLoader rootPath = Loader (go Object.empty [] <<< parseDeps "<file>")
  where
  moduleCache :: Ref (Object Module)
  moduleCache = unsafePerformEffect (Ref.new Object.empty)

  putModule :: String -> Module -> Effect Unit
  putModule a b = Ref.modify_ (Object.insert a b) moduleCache

  getModule :: String -> Effect (Maybe Module)
  getModule a = Object.lookup a <$> Ref.read moduleCache

  load :: Dependency -> ExceptT String (ContT Unit Effect) Module
  load { name, path } = do
    cached <- liftEffect $ getModule name
    case cached of
      Just mod -> pure mod
      Nothing -> do
        mod <-
          case path of
            Just path' -> do
              srcStr <- API.get (rootPath <> "/" <> path')
              let src = JS $ srcStr <> "\n//@ sourceURL=" <> path'
              pure { name, path, deps: parseDeps name src, src }
            Nothing ->
              pure { name, path, deps: [], src: ffiDep name }
        liftEffect $ putModule name mod
        pure mod

  go :: Object JS -> Array String -> Array Dependency -> ExceptT String (ContT Unit Effect) LoaderResult
  go ms ffi []   = pure { modules: ms, ffiDeps: resolveShims ffi }
  go ms ffi deps = do
    modules <- parTraverse load deps
    let
      ms' =
        modules
          # map (\{ name, src } -> Tuple name src)
          # Object.fromFoldable
          # Object.union ms
      ffi' =
        modules
          # Array.mapMaybe case _ of
              { name, path: Nothing } -> Just name
              _ -> Nothing
          # append ffi
    modules
      # bindFlipped _.deps
      # Array.nubBy (comparing _.name)
      # go ms' ffi'

ffiDep :: String -> JS
ffiDep name =
  case Object.lookup name shims of
    Just { shim } -> shim
    Nothing -> JS $ "throw new Error('FFI dependency not provided: " <> name <> "');"

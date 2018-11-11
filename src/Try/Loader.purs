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
import Data.Maybe (Maybe(..))
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
import Try.Types (JS(..))

type Module =
  { name :: String
  , path :: String
  , deps :: Array Dependency
  , src :: JS
  }

type Dependency =
  { name :: String
  , path :: String
  }

requireRegex :: Regex
requireRegex = unsafeRegex """^var\s+\S+\s*=\s*require\(["']([^"']*)["']\);""" noFlags

parseDeps :: String -> JS -> Array Dependency
parseDeps current = Array.mapMaybe go <<< String.split (Pattern "\n") <<< unwrap
  where
  go :: String -> Maybe Dependency
  go line = do
    match <- Regex.match requireRegex line
    requirePath <- join $ NonEmpty.index match 1
    case String.split (Pattern "/") requirePath of
      [ ".", "foreign.js" ] ->
        Just
          { name: current <> "$Foreign"
          , path: current <> "/foreign.js"
          }
      [ "..", name, "index.js" ] ->
        Just
          { name
          , path: name <> "/index.js"
          }
      _ ->
        Nothing

newtype Loader = Loader (JS -> ExceptT String (ContT Unit Effect) (Object JS))

runLoader :: Loader -> JS -> ExceptT String (ContT Unit Effect) (Object JS)
runLoader (Loader k) = k

makeLoader :: (Module -> Module) -> String -> Loader
makeLoader modFn rootPath = Loader (go Object.empty <<< parseDeps "<file>")
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
        src <- JS <$> API.get (rootPath <> "/" <> path)
        let mod = modFn { name, path, deps: parseDeps name src, src }
        liftEffect $ putModule name mod
        pure mod

  go :: Object JS -> Array Dependency -> ExceptT String (ContT Unit Effect) (Object JS)
  go accum []   = pure accum
  go accum deps = do
    modules <- parTraverse load deps
    let
      accum' =
        modules
          # map (\{ name, src } -> Tuple name src)
          # Object.fromFoldable
          # Object.union accum
    modules
      # bindFlipped _.deps
      # Array.nubBy (comparing _.name)
      # go accum'

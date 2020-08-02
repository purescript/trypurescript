module TPS.Loader where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import TPS.Config (loaderUrl)
import Control.Bind (bindFlipped)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import TPS.Shim (shims)
import TPS.Types (JS(..))

{-
Collects all JS modules required by compled code.
-}
--
type Module
  = { name :: String
    , path :: Maybe String
    , deps :: Array Dependency
    , src :: JS
    }

type Dependency
  = { name :: String
    , path :: Maybe String
    }

requireRegex :: Regex
requireRegex = unsafeRegex """^var\s+\S+\s*=\s*require\(["']([^"']*)["']\)""" noFlags

-- Consider replacing these with node-path dirname and concat
dirname :: String -> String
dirname path =
  fromMaybe "" do
    ix <- String.lastIndexOf (Pattern "/") path
    pure $ String.take ix path

resolvePath :: String -> String -> Maybe String
resolvePath a b
  | String.take 2 b == "./" = Just $ dirname a <> String.drop 1 b
  | String.take 3 b == "../" = Just $ dirname (dirname a) <> String.drop 2 b
  | otherwise = Nothing

parseDeps :: String -> JS -> Array Dependency
parseDeps current = Array.mapMaybe go <<< String.split (Pattern "\n") <<< unwrap
  where
  go :: String -> Maybe Dependency
  go line = do
    match <- Regex.match requireRegex line
    requirePath <- join $ NonEmpty.index match 1
    pure
      $ case resolvePath current requirePath of
          Just path ->
            { name: path
            , path: String.stripPrefix (Pattern "/") path
            }
          _ ->
            { name: requirePath
            , path: Nothing
            }

{-
Notes

Could change error handling, but kinda nice to
just throw the errors from JS.

Assuming makeLoader runLoader pattern is to save
cache between calls to runLoader.

-}
newtype Loader
  = Loader (JS -> Aff (Object JS))

runLoader :: Loader -> JS -> Aff (Object JS)
runLoader (Loader k) js = do
  -- Run loader to collect all dependencies for compiled code
  obj <- k js
  -- Return dependencies along with compiled code
  pure $ Object.insert "<file>" js obj

makeLoader :: Loader
makeLoader = Loader (go Object.empty <<< parseDeps "<file>")
  where
  moduleCache :: Ref (Object Module)
  moduleCache = unsafePerformEffect (Ref.new Object.empty)

  putModule :: String -> Module -> Effect Unit
  putModule a b = Ref.modify_ (Object.insert a b) moduleCache

  getModule :: String -> Effect (Maybe Module)
  getModule a = Object.lookup a <$> Ref.read moduleCache

  load :: Dependency -> Aff Module
  load { name, path } = do
    cached <- liftEffect $ getModule name
    case cached of
      Just mod -> pure mod
      Nothing -> do
        mod <- case path of
          -- Path means dependency is another file
          Just path' -> do
            let
              url = loaderUrl <> "/" <> path'
            --log $ "get: " <> url
            res <- AX.get AXRF.string url
            case res of
              Left err -> pure { name, path, deps: [], src }
                where
                src = throwJSError $ "Could not get file " <> url <> ", " <> AX.printError err
              Right { body } -> do
                --log $ "got body:\n" <> body
                pure { name, path, deps: parseDeps name src, src }
                where
                src = JS $ body <> "\n//# sourceURL=" <> path'
          -- No path means dependency is a shim
          Nothing -> case Object.lookup name shims of
            Just shim -> do
              res <- AX.get AXRF.string shim.url
              case res of
                Left err -> pure { name, path, deps: [], src }
                  where
                  src = throwJSError $ "Could not get shim " <> name <> " at " <> shim.url <> ", " <> AX.printError err
                Right { body } -> pure { name, path, deps, src }
                  where
                  src = JS $ body <> "\n//# sourceURL=" <> shim.url

                  deps = { name: _, path: Nothing } <$> shim.deps
            Nothing -> pure { name, path, deps: [], src }
              where
              -- Todo - link to instructions for adding shims
              src = throwJSError $ "FFI dependency not provided: " <> name
        liftEffect $ putModule name mod
        pure mod

  go :: Object JS -> Array Dependency -> Aff (Object JS)
  go ms [] = pure ms

  go ms deps = do
    modules <- parTraverse load deps
    let
      ms' =
        modules
          # map (\{ name, src } -> Tuple name src)
          # Object.fromFoldable
          # Object.union ms
    modules
      # bindFlipped _.deps
      # Array.nubBy (comparing _.name)
      # go ms'

throwJSError :: String -> JS
throwJSError err = JS $ "throw new Error('" <> err <> "');"

module Try.Backend
  ( Backend(..)
  , BackendConfig(..)
  , getBackendConfig
  , getBackendConfigFromString
  ) where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Parallel (parTraverse)
import Data.Either (Either)
import Data.Foldable (fold, intercalate)
import Data.List.NonEmpty (NonEmptyList)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Foreign (ForeignError)
import Partial.Unsafe (unsafePartial)
import Try.API (CompileResult, compile, get)
import Try.Loader (Loader, makeLoader)
import Try.Types (JS(..))

newtype BackendConfig = BackendConfig
  { backend       :: String
  , mainGist      :: String
  , extra_styling :: String
  , extra_body    :: String
  , loader        :: Loader
  , compile       :: String
                  -> ExceptT String (ContT Unit Effect)
                       (Either (NonEmptyList ForeignError) CompileResult)
  , getBundle     :: ExceptT String (ContT Unit Effect) JS
  }

data Backend
  = Core
  | Thermite
  | Slides
  | Mathbox
  | Behaviors
  | Flare

backendFromString :: Partial => String -> Backend
backendFromString "core"      = Core
backendFromString "thermite"  = Thermite
backendFromString "slides"    = Slides
backendFromString "mathbox"   = Mathbox
backendFromString "behaviors" = Behaviors
backendFromString "flare"     = Flare

backendToString :: Backend -> String
backendToString Core      = "core"
backendToString Thermite  = "thermite"
backendToString Slides    = "slides"
backendToString Mathbox   = "mathbox"
backendToString Behaviors = "behaviors"
backendToString Flare     = "flare"

derive instance eqBackend :: Eq Backend
derive instance ordBackend :: Ord Backend

-- | Get the default bundle
getDefaultBundle
  :: String
  -> ExceptT String (ContT Unit Effect) JS
getDefaultBundle endpoint = JS <$> get (endpoint <> "/bundle")

-- | Get the JS bundle for the Thermite backend, which includes additional dependencies
getThermiteBundle
  :: String
  -> ExceptT String (ContT Unit Effect) JS
getThermiteBundle endpoint =
  let getAll = parTraverse get
        [ "js/console.js"
        , "js/react.min.js"
        , "js/react-dom.min.js"
        , endpoint <> "/bundle"
        ]

      onComplete :: Partial
                 => Array String
                 -> JS
      onComplete [consoleScript, react, react_dom, bundle] =
        let replaced = bundle
                         # replace (unsafeRegex """require\("react"\)""" global) "window.React"
                         # replace (unsafeRegex """require\("react-dom"\)""" global) "window.ReactDOM"
                         # replace (unsafeRegex """require\("react-dom\/server"\)""" global) "window.ReactDOM"
        in JS (intercalate "\n" [consoleScript, react, react_dom, replaced])
  in unsafePartial onComplete <$> getAll

-- TODO: Fix paths
getBackendConfig :: Backend -> BackendConfig
getBackendConfig Core = BackendConfig
  { backend: "core"
  , mainGist: "b57a766d417e109785540d584266fc33"
  , extra_styling: ""
  , extra_body: ""
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "http://localhost:8081"
  , getBundle: getDefaultBundle "http://localhost:8081"
  }
getBackendConfig Thermite = BackendConfig
  { backend: "thermite"
  , mainGist: "85383bb058471109cfef379bbb6bc11c"
  , extra_styling: """<link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">"""
  , extra_body: """<div id="app"></div>"""
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "https://compile.purescript.org/thermite"
  , getBundle: getThermiteBundle "https://compile.purescript.org/thermite"
  }
getBackendConfig Slides = BackendConfig
  { backend: "slides"
  , mainGist: "c62b5778a6a5f2bcd32dd97b294c068a"
  , extra_styling: """<link rel="stylesheet" href="css/slides.css">"""
  , extra_body: """<div id="main"></div>"""
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "https://compile.purescript.org/slides"
  , getBundle: getDefaultBundle "https://compile.purescript.org/slides"
  }
getBackendConfig Mathbox = BackendConfig
  { backend: "mathbox"
  , mainGist: "81f8bb3261b9c819d677de2ea54a4d2e"
  , extra_styling: fold
      [ """<script src="js/mathbox-bundle.js"></script>"""
      , """<link rel="stylesheet" href="css/mathbox.css">"""
      ]
  , extra_body: ""
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "https://compile.purescript.org/purescript-mathbox"
  , getBundle: getDefaultBundle "https://compile.purescript.org/purescript-mathbox"
  }
getBackendConfig Behaviors = BackendConfig
  { backend: "behaviors"
  , mainGist: "ff1e87f0872d2d891e77d209d8f7706d"
  , extra_styling: ""
  , extra_body: """<canvas id="canvas" width="800" height="600"></canvas>"""
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "https://compile.purescript.org/behaviors"
  , getBundle: getDefaultBundle "https://compile.purescript.org/behaviors"
  }
getBackendConfig Flare = BackendConfig
  { backend: "flare"
  , mainGist: "4f54d6dd213caa54d736ead597e17fee"
  , extra_styling: """<link rel="stylesheet" href="css/flare.css">"""
  , extra_body: fold
      [ """<div id="controls"></div>"""
      , """<div id="output"></div>"""
      , """<div id="tests"></div>"""
      , """<canvas id="canvas" width="800" height="600"></canvas>"""
      ]
  , loader: makeLoader "staging/core/.psci_modules/node_modules"
  , compile: compile "https://compile.purescript.org/flare"
  , getBundle: getDefaultBundle "https://compile.purescript.org/flare"
  }

getBackendConfigFromString :: String -> BackendConfig
getBackendConfigFromString s = getBackendConfig (unsafePartial backendFromString s)

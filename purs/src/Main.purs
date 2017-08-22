module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery, on, ready, select, toggleClass)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, mkEffFn1, mkEffFn2, runEffFn1, runEffFn2, runEffFn4)
import DOM (DOM)
import Data.Foldable (fold)
import Partial.Unsafe (unsafePartial)

-- | A record of functions that we export to the foreign JavaScript code
newtype ExportedFunctions = ExportedFunctions
  { getBackend :: String -> BackendConfig
  , setupEditor :: forall eff. EffFn2 (dom :: DOM | eff) ExportedFunctions BackendConfig Unit
  }

exportedFunctions :: ExportedFunctions
exportedFunctions = ExportedFunctions
  { getBackend: getBackendFromString
  , setupEditor: setupEditor
  }

-- | An abstract data type representing the data we get back from the GitHub API.
data GistInfo

init :: forall eff. Eff (dom :: DOM | eff) Unit
init = do
  select "#showjs" >>= on "change" \e _ -> runEffFn1 compile exportedFunctions
  select "#compile_label" >>= on "click" \e _ -> runEffFn1 compile exportedFunctions

  select "input[name=view_mode]" >>= on "change" \_ jq -> runEffFn1 changeViewMode jq

  select "#gist_save" >>= on "click" \e _ -> publishNewGist

  select "#hamburger" >>= on "click" \e _ -> do
    select "#menu" >>= toggleClass "show"

  select "#view_mode_label" >>= on "click" \e _ -> do
    select "#view_mode" >>= toggleClass "show-sub-menu"

  select "#backend_label" >>= on "click" \e _ -> do
    select "#backend" >>= toggleClass "show-sub-menu"

  select "#editor_view" >>= on "click" \e _ -> hideMenus

  runEffFn1 setupSession (mkEffFn1 \x -> runEffFn2 withSession exportedFunctions x)

setupEditor :: forall eff. EffFn2 (dom :: DOM | eff) ExportedFunctions BackendConfig Unit
setupEditor = mkEffFn2 \exports backend -> do
  runEffFn2 loadOptions exports backend
  runEffFn4 setupEditorWith exports "code" "code_textarea" "ace/mode/haskell"
  runEffFn1 cacheCurrentCode backend

-- | Set up AJAX parameters in Jquery for future AJAX calls.
foreign import setupAjax :: forall eff. Eff (dom :: DOM | eff) Unit

-- | Add plugins to the global Jquery object.
foreign import setupJqueryPlugins :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import changeViewMode :: forall eff. EffFn1 (dom :: DOM | eff) JQuery Unit

foreign import defaultBundleAndExecute :: forall eff. EffFn2 (dom :: DOM | eff) JS BackendConfig Unit

foreign import bundleAndExecuteThermite :: forall eff. EffFn2 (dom :: DOM | eff) JS BackendConfig Unit

foreign import cacheCurrentCode :: forall eff. EffFn1 (dom :: DOM | eff) BackendConfig Unit

foreign import compile :: forall eff. EffFn1 (dom :: DOM | eff) ExportedFunctions Unit

foreign import execute :: forall eff. EffFn3 (dom :: DOM | eff) JS JS BackendConfig Unit

foreign import hideMenus :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import loadFromGist :: forall eff. EffFn3 (dom :: DOM | eff) ExportedFunctions String BackendConfig Unit

foreign import loadOptions :: forall eff. EffFn2 (dom :: DOM | eff) ExportedFunctions BackendConfig Unit

foreign import publishNewGist :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import setupEditorWith :: forall eff. EffFn4 (dom :: DOM | eff) ExportedFunctions String String String Unit

foreign import setupSession :: forall eff. EffFn1 (dom :: DOM | eff) (EffFn1 (dom :: DOM | eff) String Unit) Unit

foreign import tryLoadFileFromGist :: forall eff. EffFn2 (dom :: DOM | eff) GistInfo String Unit

foreign import tryRestoreCachedCode :: forall eff. EffFn1 (dom :: DOM | eff) String Unit

foreign import withSession :: forall eff. EffFn2 (dom :: DOM | eff) ExportedFunctions String Unit

newtype JS = JS String

getJS :: JS -> String
getJS (JS js) = js

newtype BackendConfig = BackendConfig
  { backend          :: String
  , endpoint         :: String
  , mainGist         :: String
  , extra_styling    :: String
  , extra_body       :: String
  , bundleAndExecute :: forall eff. EffFn2 (dom :: DOM | eff) JS BackendConfig Unit
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

getBackend :: Backend -> BackendConfig
getBackend Core      = BackendConfig
  { backend: "core"
  , endpoint: "https://compile.purescript.org/try"
  , mainGist: "b57a766d417e109785540d584266fc33"
  , extra_styling: ""
  , extra_body: ""
  , bundleAndExecute: defaultBundleAndExecute
  }
getBackend Thermite  = BackendConfig
  { backend: "thermite"
  , endpoint: "https://compile.purescript.org/thermite"
  , mainGist: "85383bb058471109cfef379bbb6bc11c"
  , extra_styling: "<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css\">"
  , extra_body: "<div id=\"app\"></div>"
  , bundleAndExecute: bundleAndExecuteThermite
  }
getBackend Slides    = BackendConfig
  { backend: "slides"
  , endpoint: "https://compile.purescript.org/slides"
  , mainGist: "c62b5778a6a5f2bcd32dd97b294c068a"
  , extra_styling: "<link rel=\"stylesheet\" href=\"css/slides.css\">"
  , extra_body: "<div id=\"main\"></div>"
  , bundleAndExecute: defaultBundleAndExecute
  }
getBackend Mathbox   = BackendConfig
  { backend: "mathbox"
  , endpoint: "https://compile.purescript.org/purescript-mathbox"
  , mainGist: "aeecffd458fa8a365b4af3b3cd9d7759"
  , extra_styling: fold
      [ "<script src=\"js/mathbox-bundle.js\"></script>"
      , "<link rel=\"stylesheet\" href=\"css/mathbox.css\">"
      ]
  , extra_body: ""
  , bundleAndExecute: defaultBundleAndExecute
  }
getBackend Behaviors = BackendConfig
  { backend: "behaviors"
  , endpoint: "https://compile.purescript.org/behaviors"
  , mainGist: "ff1e87f0872d2d891e77d209d8f7706d"
  , extra_styling: ""
  , extra_body: "<canvas id=\"canvas\" width=\"800\" height=\"600\"></canvas>"
  , bundleAndExecute: defaultBundleAndExecute
  }
getBackend Flare     = BackendConfig
  { backend: "flare"
  , endpoint: "https://compile.purescript.org/flare"
  , mainGist: "4f54d6dd213caa54d736ead597e17fee"
  , extra_styling: "<link rel=\"stylesheet\" href=\"css/flare.css\">"
  , extra_body: fold
      [ "<div id=\"controls\"></div>"
      , "<div id=\"output\"></div>"
      , "<div id=\"tests\"></div>"
      , "<canvas id=\"canvas\" width=\"800\" height=\"600\"></canvas>"
      ]
  , bundleAndExecute: defaultBundleAndExecute
  }

getBackendFromString :: String -> BackendConfig
getBackendFromString s = getBackend (unsafePartial backendFromString s)

main :: Eff (dom :: DOM) Unit
main = do
  setupAjax
  ready do
    setupJqueryPlugins
    init

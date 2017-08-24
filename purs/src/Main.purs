module Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.JQuery (JQuery, Selector, attr, display, hide, on, ready, removeClass, select, setProp, setValue, toggleClass)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, mkEffFn1, mkEffFn2, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (elem, fold, intercalate)
import Data.Functor.App (App(..))
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String (drop, length)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

-- | A record of functions that we export to the foreign JavaScript code
newtype ExportedFunctions = ExportedFunctions
  { getBackend :: String -> BackendConfig
  }

exportedFunctions :: ExportedFunctions
exportedFunctions = ExportedFunctions
  { getBackend: getBackendFromString
  }

-- | An abstract data type representing the data we get back from the GitHub API.
data GistInfo

-- | Get a gist by its ID
foreign import getGistById
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) GistInfo Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `getGistById` which uses `ContT`.
getGistByIdContT :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) GistInfo
getGistByIdContT id_ = ExceptT (ContT \k -> runEffFn3 getGistById id_ (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

setupEditor :: forall eff. EffFn2 (dom :: DOM, timer :: TIMER | eff) ExportedFunctions BackendConfig Unit
setupEditor = mkEffFn2 \exports backend -> do
  runEffFn1 loadOptions backend
  runEffFn4 setupEditorWith exports "code" "code_textarea" "ace/mode/haskell"
  runEffFn1 cacheCurrentCode backend

defaultBundleAndExecute :: forall eff. EffFn2 (console :: CONSOLE, dom :: DOM | eff) JS BackendConfig Unit
defaultBundleAndExecute = mkEffFn2 \js bc@(BackendConfig backend) -> do
  runEffFn3 get
    (backend.endpoint <> "/bundle")
    (mkEffFn1 \bundle -> runEffFn3 execute js (JS bundle) bc)
    (mkEffFn1 \err -> warn ("Unable to load JS bundle: " <> err))

bundleAndExecuteThermite :: forall eff. EffFn2 (console :: CONSOLE, dom :: DOM | eff) JS BackendConfig Unit
bundleAndExecuteThermite =
  mkEffFn2 \js bc@(BackendConfig backend) ->
    let getAll = parTraverse getContT
          [ "js/console.js"
          , "js/react.min.js"
          , "js/react-dom.min.js"
          , backend.endpoint <> "/bundle"
          ]

        onComplete :: Partial
                   => Either String (Array String)
                   -> Eff ( console :: CONSOLE
                          , dom :: DOM
                          | eff
                          ) Unit
        onComplete (Left err) = warn ("Unable to load JS bundle: " <> err)
        onComplete (Right [consoleScript, react, react_dom, bundle]) =
          let replaced = bundle
                           # replace (unsafeRegex "require\\(\"react\"\\)" global) "window.React"
                           # replace (unsafeRegex "require\\(\"react-dom\"\\)" global) "window.ReactDOM"
                           # replace (unsafeRegex "require\\(\"react-dom\\/server\"\\)" global) "window.ReactDOM"
          in runEffFn3 execute js (JS (intercalate "\n" [consoleScript, react, react_dom, replaced])) bc
    in runContT (runExceptT getAll) (unsafePartial onComplete)

loadFromGist :: forall eff. EffFn2 (console :: CONSOLE, dom :: DOM, timer :: TIMER | eff) String BackendConfig Unit
loadFromGist = mkEffFn2 \id_ backend -> do
  runContT (runExceptT (getGistByIdContT id_ >>= \gi -> tryLoadFileFromGistContT gi "Main.purs")) $
    case _ of
      Left err -> do
        log ("Unable to load gist metadata or contents: " <> err)
        runEffFn2 setupEditor exportedFunctions backend
      Right code -> do
        select "#code_textarea" >>= setValue code
        runEffFn2 setupEditor exportedFunctions backend

tryRestoreCachedCodeMaybe :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe String)
tryRestoreCachedCodeMaybe = map toMaybe <<< runEffFn1 tryRestoreCachedCode

withSession :: forall eff. EffFn1 (console :: CONSOLE, dom :: DOM, timer :: TIMER | eff) String Unit
withSession = mkEffFn1 \sessionId -> do
  cachedBackend <- tryRestoreCachedCodeMaybe sessionId
  case cachedBackend of
    Just cachedBackend_ -> runEffFn2 setupEditor exportedFunctions (getBackendFromString cachedBackend_)
    Nothing -> do
      bc@(BackendConfig backend) <- getBackendFromString <<< fromMaybe "core" <$> getQueryStringMaybe "backend"
      gist <- fromMaybe backend.mainGist <$> getQueryStringMaybe "gist"
      runEffFn2 loadFromGist gist bc

foreign import get
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

-- | A wrapper for `get` which uses `ContT`.
getContT :: forall eff. String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
getContT uri = ExceptT (ContT \k -> runEffFn3 get uri (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

foreign import cacheCurrentCode :: forall eff. EffFn1 (dom :: DOM | eff) BackendConfig Unit

foreign import compile :: forall eff. EffFn1 (dom :: DOM | eff) ExportedFunctions Unit

foreign import execute :: forall eff. EffFn3 (dom :: DOM | eff) JS JS BackendConfig Unit

foreign import publishNewGist :: forall eff. Eff (dom :: DOM | eff) Unit

foreign import setupEditorWith :: forall eff. EffFn4 (dom :: DOM | eff) ExportedFunctions String String String Unit

randomGuid :: forall eff. Eff (random :: RANDOM | eff) String
randomGuid =
    unwrap (App s4 <> App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> pure "-" <>
            App s4 <> App s4 <> App s4)
  where
    s4 = padLeft <<< toStringAs hexadecimal <$> randomInt 0 (256 * 256)
    padLeft s = drop (length s - 1) ("000" <> s)

-- | Look up the session by ID, or create a new session ID.
setupSession :: forall eff. EffFn1 (dom :: DOM, random :: RANDOM | eff) (EffFn1 (dom :: DOM, random :: RANDOM | eff) String Unit) Unit
setupSession = mkEffFn1 \k -> do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> runEffFn1 k sessionId_
    Nothing -> randomGuid >>= runEffFn2 setQueryString "session"

foreign import tryLoadFileFromGist
  :: forall eff
   . EffFn4 (dom :: DOM | eff)
            GistInfo
            String
            (EffFn1 (dom :: DOM | eff) String Unit)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Unit

tryLoadFileFromGistContT :: forall eff. GistInfo -> String -> ExceptT String (ContT Unit (Eff (dom :: DOM | eff))) String
tryLoadFileFromGistContT gi filename = ExceptT (ContT \k -> runEffFn4 tryLoadFileFromGist gi filename (mkEffFn1 (k <<< Right)) (mkEffFn1 (k <<< Left)))

foreign import tryRestoreCachedCode :: forall eff. EffFn1 (dom :: DOM | eff) String (Nullable String)

-- | Get the value of a query string parameter from the jQuery plugin.
foreign import getQueryString :: forall eff. EffFn1 (dom :: DOM | eff) String (Nullable String)

getQueryStringMaybe :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe String)
getQueryStringMaybe = map toMaybe <<< runEffFn1 getQueryString

-- | Set the value of a query string parameter
foreign import setQueryString :: forall eff. EffFn2 (dom :: DOM | eff) String String Unit

-- | Simulate a click event on the specified element.
foreign import click :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit

-- | Filter elements based on an additional selector.
foreign import filter :: forall eff. EffFn2 (dom :: DOM | eff) JQuery Selector JQuery

-- | Get the value of the first element, if it exists.
foreign import getValue :: forall eff. EffFn1 (dom :: DOM | eff) JQuery (Nullable String)

-- | Present the user with a confirmation dialog
foreign import confirm :: forall eff. String -> Eff (dom :: DOM | eff) Boolean

-- | Navigate to the specified URL.
foreign import navigateTo :: forall eff. String -> Eff (dom :: DOM | eff) Unit

getValueMaybe :: forall eff. JQuery -> Eff (dom :: DOM | eff) (Maybe String)
getValueMaybe = map toMaybe <<< runEffFn1 getValue

-- | Hide the drop down menus
hideMenus :: forall eff. Eff (dom :: DOM | eff) Unit
hideMenus = do
  select "#menu" >>= removeClass "show"
  select "#view_mode" >>= removeClass "show-sub-menu"
  select "#backend" >>= removeClass "show-sub-menu"

-- | Update the view mode based on the menu selection
changeViewMode :: forall eff. EffFn1 (dom :: DOM | eff) JQuery Unit
changeViewMode = mkEffFn1 \jq -> do
  viewMode <- runEffFn2 filter jq ":checked" >>= getValueMaybe
  case viewMode of
    Just "code" -> do
      select "#column1" >>= display
      select "#column2" >>= hide
      select "#showjs_label" >>= hide
      select "#showjs" >>= hide
    Just "output" -> do
      select "#column1" >>= hide
      select "#column2" >>= display
      select "#showjs_label" >>= display
      select "#showjs" >>= display
    _ -> do
      select "#column1" >>= display
      select "#column2" >>= display
      select "#showjs_label" >>= display
      select "#showjs" >>= display

-- | Read query string options and update the state accordingly
loadOptions :: forall eff. EffFn1 (dom :: DOM, timer :: TIMER | eff) BackendConfig Unit
loadOptions = mkEffFn1 \bc@(BackendConfig backend) -> do
  select ("#backend_" <> backend.backend) >>= attr { checked: "checked" }

  viewMode <- getQueryStringMaybe "view"
  case viewMode of
    Just viewMode_
      | viewMode_ `elem` ["sidebyside", "code", "output"]
      -> select ("#view_" <> viewMode_) >>= click
    _ -> pure unit

  showJs <- getQueryStringMaybe "js"
  case showJs of
    Just showJs_ ->
      select "input:checkbox[name=showjs]" >>= setProp "checked" (showJs_ == "true")
    _ -> pure unit

  autoCompile <- getQueryStringMaybe "compile"
  case autoCompile of
    Just autoCompile_ ->
      select "input:checkbox[name=auto_compile]" >>= setProp "checked" (autoCompile_ == "true")
    _ -> pure unit

  gist <- getQueryStringMaybe "gist"
  case gist of
    Just gist_ -> select "#view_gist" >>= attr { href: "https://gist.github.com/" <> gist_ }
    Nothing -> select "#view_gist_li" >>= hide

  select "input[name=backend_inputs]" >>= on "change" \e jq -> do
    bc_@(BackendConfig newBackend) <- getBackendFromString <$> map (fromMaybe "core") (runEffFn2 filter jq ":checked" >>= getValueMaybe)

    ok <- confirm ("Replace your current code with the " <> newBackend.backend <> " backend sample code?")
    if ok
      then navigateTo ("?backend=" <> newBackend.backend)
      else void $ setTimeout 1000 do
             runEffFn1 compile exportedFunctions
             runEffFn1 cacheCurrentCode bc_
    hideMenus

newtype JS = JS String

getJS :: JS -> String
getJS (JS js) = js

newtype BackendConfig = BackendConfig
  { backend          :: String
  , endpoint         :: String
  , mainGist         :: String
  , extra_styling    :: String
  , extra_body       :: String
  , bundleAndExecute :: forall eff. EffFn2 (console :: CONSOLE, dom :: DOM | eff) JS BackendConfig Unit
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

main :: Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER, random :: RANDOM) Unit
main = ready do
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

  runEffFn1 setupSession withSession

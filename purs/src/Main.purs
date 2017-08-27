module Main where

import Prelude

import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.JQuery (JQuery, addClass, append, appendText, attr, create, display, hide, on, ready, removeClass, select, setProp, setValue, toggleClass) as JQuery
import Control.Monad.Eff.JQuery.Extras (click, empty, filter, getValueMaybe, is) as JQuery
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn4, mkEffFn1, mkEffFn2, runEffFn1, runEffFn2, runEffFn4)
import Control.Monad.Except.Trans (runExceptT)
import Control.Parallel (parTraverse)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (setHref)
import DOM.HTML.Types (ALERT, CONFIRM)
import DOM.HTML.Window (alert, confirm, location)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, fold, for_, intercalate, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Foreign (renderForeignError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String as String
import Data.String.Regex (replace, replace')
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Try.API (CompileError(..), CompileResult(..), CompilerError(..), ErrorPosition(..), FailedResult(..), SuccessResult(..), get)
import Try.API as API
import Try.Gist (getGistById, tryLoadFileFromGist, uploadGist)
import Try.QueryString (getQueryStringMaybe, setQueryString)
import Try.Session (createSessionIdIfNecessary, storeSession, tryRetrieveSession)
import Try.Types (Backend(..), BackendConfig(..), JS(..), backendFromString)

-- | Compile the current code and execute it.
compile :: forall eff. BackendConfig -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
compile bc@(BackendConfig backend) = do
  JQuery.select "#column2" >>= \jq -> do
    JQuery.empty jq
    JQuery.create "<div>" >>= \div -> do
      JQuery.addClass "loading" div
      JQuery.appendText "Loading..." div
      JQuery.append div jq

  code <- fold <$> (JQuery.select "#code_textarea" >>= JQuery.getValueMaybe)

  let displayPlainText s =
        JQuery.select "#column2" >>= \jq -> do
          JQuery.empty jq
          JQuery.create "<pre>" >>= \pre -> do
            JQuery.create "<code>" >>= \code_ -> do
              JQuery.append code_ pre
              JQuery.appendText s code_
              JQuery.append pre jq

  runContT (runExceptT (API.compile bc code)) \res_ ->
    case res_ of
      Left err -> displayPlainText err
      Right res -> do
        cleanUpMarkers

        case res of
          Right (CompileSuccess (SuccessResult { js })) -> do
            showJs <- JQuery.select "#showjs" >>= \jq -> JQuery.is jq ":checked"
            if showJs
              then displayPlainText js
              else backend.bundleAndExecute (JS js) bc
          Right (CompileFailed (FailedResult { error })) ->
            case error of
              CompilerErrors errs -> do
                JQuery.select "#column2" >>= JQuery.empty
                forWithIndex_ errs \i (CompilerError{ message, position: ErrorPosition pos }) -> do
                  JQuery.select "#column2" >>= \jq -> do
                    h1 <- JQuery.create "<h1>"
                    JQuery.addClass "error-banner" h1
                    JQuery.appendText ("Error " <> show (i + 1) <> " of " <> show (Array.length errs)) h1

                    pre <- JQuery.create "<pre>"
                    code_ <- JQuery.create "<code>"
                    JQuery.append code_ pre
                    JQuery.appendText message code_

                    JQuery.append h1 jq
                    JQuery.append pre jq

                  runEffFn4 addErrorMarker
                    pos.startLine
                    pos.startColumn
                    pos.endLine
                    pos.endColumn
              OtherError err -> displayPlainText err
          Left errs -> do
            displayPlainText "Unable to parse the response from the server"
            traverse_ (error <<< renderForeignError) errs

-- | Set up a fresh iframe in the specified container, and use it
-- | to execute the provided JavaScript code.
foreign import setupIFrame
  :: forall eff
   . EffFn4 (dom :: DOM | eff)
            JQuery.JQuery
            String
            String
            (EffFn1 (dom :: DOM | eff) JQuery.JQuery Unit)
            Unit

-- | Execute the compiled code in a new iframe.
execute :: forall eff. JS -> JS -> BackendConfig -> Eff (dom :: DOM | eff) Unit
execute js bundle bc@(BackendConfig backend) = do
  let html = joinWith "\n"
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <meta content=\"text/html;charset=utf-8\" http-equiv=\"Content-Type\">"
        , "    <meta content=\"utf-8\" http-equiv=\"encoding\">"
        , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "    <title>Try PureScript!</title>"
        , "    <link rel=\"stylesheet\" href=\"css/style.css\">"
        , backend.extra_styling
        , "  </head>"
        , "  <body>"
        , backend.extra_body
        , "  </body>"
        , "</html>"
        ]

      replaced = replace' (unsafeRegex """require\("[^"]*"\)""" global) (\s _ ->
        "PS['" <> String.drop 12 (String.take (String.length s - 2) s) <> "']") (unwrap js)

      wrapped = joinWith "\n"
        [ "var module = {};"
        , "(function(module) {"
        , replaced
        , "})(module);"
        , "module.exports.main && module.exports.main();"
        ]

      scripts = joinWith "\n" [unwrap bundle, wrapped]

  JQuery.select "#column2" >>= \ctr ->
    runEffFn4 setupIFrame ctr html scripts $ mkEffFn1 \body ->
      JQuery.on "click" (\_ _ -> hideMenus) body

-- | Set the editor content to the specified string.
foreign import setEditorContent :: forall eff. EffFn1 (dom :: DOM | eff) String Unit

-- | Register a callback for editor change events.
foreign import onEditorChanged
  :: forall eff
   . EffFn2 (dom :: DOM | eff)
            (EffFn1 (dom :: DOM | eff) String Unit)
            Int
            Unit

-- | Clean up any global state associated with any visible error markers.
foreign import cleanUpMarkers :: forall eff. Eff (dom :: DOM | eff) Unit

-- | Add a visible error marker at the specified location.
foreign import addErrorMarker :: forall eff. EffFn4 (dom :: DOM | eff) Int Int Int Int Unit

-- | Set up the editor content, and registers a callback for any changes.
setupEditorWith :: forall eff. BackendConfig -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEditorWith bc = do
  JQuery.select "#code_textarea"
    >>= JQuery.getValueMaybe
    >>= fold
    >>> runEffFn1 setEditorContent

  runEffFn2 onEditorChanged (mkEffFn1 \value -> do
     JQuery.select "#code_textarea" >>= JQuery.setValue value
     cacheCurrentCode
     autoCompile <- JQuery.select "#auto_compile" >>= \jq -> JQuery.is jq ":checked"
     when autoCompile do
       compile bc) 750

  compile bc

setupEditor
  :: forall eff
   . EffFn1 ( console :: CONSOLE
            , confirm :: CONFIRM
            , dom :: DOM
            , timer :: TIMER
            | eff
            ) BackendConfig Unit
setupEditor = mkEffFn1 \backend -> do
  loadOptions backend
  setupEditorWith backend
  cacheCurrentCode

defaultBundleAndExecute
  :: forall eff
   . JS
  -> BackendConfig
  -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
defaultBundleAndExecute js bc@(BackendConfig backend) = do
  runContT (runExceptT (get (backend.endpoint <> "/bundle")))
    case _ of
      Left err -> error ("Unable to load JS bundle: " <> err)
      Right bundle -> execute js (JS bundle) bc

bundleAndExecuteThermite
  :: forall eff
   . JS
  -> BackendConfig
  -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
bundleAndExecuteThermite js bc@(BackendConfig backend) =
  let getAll = parTraverse get
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
      onComplete (Left err) = error ("Unable to load JS bundle: " <> err)
      onComplete (Right [consoleScript, react, react_dom, bundle]) =
        let replaced = bundle
                         # replace (unsafeRegex "require\\(\"react\"\\)" global) "window.React"
                         # replace (unsafeRegex "require\\(\"react-dom\"\\)" global) "window.ReactDOM"
                         # replace (unsafeRegex "require\\(\"react-dom\\/server\"\\)" global) "window.ReactDOM"
        in execute js (JS (intercalate "\n" [consoleScript, react, react_dom, replaced])) bc
  in runContT (runExceptT getAll) (unsafePartial onComplete)

loadFromGist :: forall eff. EffFn2 (confirm :: CONFIRM, console :: CONSOLE, dom :: DOM, timer :: TIMER | eff) String BackendConfig Unit
loadFromGist = mkEffFn2 \id_ backend -> do
  runContT (runExceptT (getGistById id_ >>= \gi -> tryLoadFileFromGist gi "Main.purs")) $
    case _ of
      Left err -> do
        error ("Unable to load gist metadata or contents: " <> err)
        runEffFn1 setupEditor backend
      Right code -> do
        JQuery.select "#code_textarea" >>= JQuery.setValue code
        runEffFn1 setupEditor backend

withSession
  :: forall eff
   . String
  -> Eff ( confirm :: CONFIRM
         , console :: CONSOLE
         , dom :: DOM
         , timer :: TIMER
         | eff
         ) Unit
withSession sessionId = do
  cachedBackend <- tryRestoreCachedCode sessionId
  case cachedBackend of
    Just cachedBackend_ -> runEffFn1 setupEditor (getBackendFromString cachedBackend_)
    Nothing -> do
      bc@(BackendConfig backend) <- getBackendFromString <<< fromMaybe "core" <$> getQueryStringMaybe "backend"
      gist <- fromMaybe backend.mainGist <$> getQueryStringMaybe "gist"
      runEffFn2 loadFromGist gist bc

-- | Cache the current code in the session state
cacheCurrentCode :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
cacheCurrentCode = do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> do
      code <- fold <$> (JQuery.select "#code_textarea" >>= JQuery.getValueMaybe)
      backend <- getBackendNameFromView
      storeSession sessionId_ { code, backend }
    Nothing -> error "No session ID"

-- | Retrieve the session state and apply it to the editor.
-- | Returns the backend name
tryRestoreCachedCode :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe String)
tryRestoreCachedCode sessionId = do
  state <- tryRetrieveSession sessionId
  for_ state \{ code, backend } -> do
    -- TODO: this needs to be improved
    JQuery.select ("#backend_" <> backend) >>= JQuery.click
    JQuery.select "#code_textarea" >>= JQuery.setValue code
  pure (map _.backend state)

-- | Create a new Gist using the current content
publishNewGist
  :: forall eff
   . Eff ( alert :: ALERT
         , confirm :: CONFIRM
         , console :: CONSOLE
         , dom :: DOM
         | eff
         ) Unit
publishNewGist = do
  ok <- window >>= confirm (intercalate "\n"
          [ "Do you really want to publish this code as an anonymous Gist?"
          , ""
          , "Note: this code will be available to anyone with a link to the Gist."
          ])
  when ok do
    content <- fold <$> (JQuery.select "#code_textarea" >>= JQuery.getValueMaybe)
    runContT (runExceptT (uploadGist content)) $
      case _ of
        Left err -> do
          window >>= alert "Failed to JQuery.create gist"
          error ("Failed to JQuery.create gist: " <> err)
        Right gistId -> do
          backend <- getBackendNameFromView
          setQueryString "gist" gistId
          setQueryString "backend" backend

-- | Navigate to the specified URL.
navigateTo :: forall eff. String -> Eff (dom :: DOM | eff) Unit
navigateTo uri = void (window >>= location >>= setHref uri)

-- | Hide the drop down menus
hideMenus :: forall eff. Eff (dom :: DOM | eff) Unit
hideMenus = do
  JQuery.select "#menu" >>= JQuery.removeClass "show"
  JQuery.select "#view_mode" >>= JQuery.removeClass "show-sub-menu"
  JQuery.select "#backend" >>= JQuery.removeClass "show-sub-menu"

-- | Update the view mode based on the menu selection
changeViewMode :: forall eff. JQuery.JQuery -> Eff (dom :: DOM | eff) Unit
changeViewMode jq = do
  viewMode <- JQuery.filter jq ":checked" >>= JQuery.getValueMaybe
  case viewMode of
    Just "code" -> do
      JQuery.select "#column1" >>= JQuery.display
      JQuery.select "#column2" >>= JQuery.hide
      JQuery.select "#showjs_label" >>= JQuery.hide
      JQuery.select "#showjs" >>= JQuery.hide
    Just "output" -> do
      JQuery.select "#column1" >>= JQuery.hide
      JQuery.select "#column2" >>= JQuery.display
      JQuery.select "#showjs_label" >>= JQuery.display
      JQuery.select "#showjs" >>= JQuery.display
    _ -> do
      JQuery.select "#column1" >>= JQuery.display
      JQuery.select "#column2" >>= JQuery.display
      JQuery.select "#showjs_label" >>= JQuery.display
      JQuery.select "#showjs" >>= JQuery.display

-- | Get the backend name from whatever is selected in the menu.
getBackendNameFromView :: forall eff. Eff (dom :: DOM | eff) String
getBackendNameFromView =
  fromMaybe "core" <$>
    (JQuery.select "input[name=backend_inputs]"
      >>= \jq -> JQuery.filter jq ":checked"
      >>= JQuery.getValueMaybe)

-- | Get the backend configuration from whatever is selected in the menu.
getBackendConfigFromView :: forall eff. Eff (dom :: DOM | eff) BackendConfig
getBackendConfigFromView = getBackendFromString <$> getBackendNameFromView

-- | Read query string options and update the state accordingly
loadOptions
  :: forall eff
   . BackendConfig
  -> Eff ( console :: CONSOLE
         , confirm :: CONFIRM
         , dom :: DOM
         , timer :: TIMER
         | eff
         ) Unit
loadOptions bc@(BackendConfig backend) = do
  JQuery.select ("#backend_" <> backend.backend) >>= JQuery.attr { checked: "checked" }

  viewMode <- getQueryStringMaybe "view"
  case viewMode of
    Just viewMode_
      | viewMode_ `elem` ["sidebyside", "code", "output"]
      -> JQuery.select ("#view_" <> viewMode_) >>= JQuery.click
    _ -> pure unit

  showJs <- getQueryStringMaybe "js"
  case showJs of
    Just showJs_ ->
      JQuery.select "input:checkbox[name=showjs]" >>= JQuery.setProp "checked" (showJs_ == "true")
    _ -> pure unit

  autoCompile <- getQueryStringMaybe "compile"
  case autoCompile of
    Just autoCompile_ ->
      JQuery.select "input:checkbox[name=auto_compile]" >>= JQuery.setProp "checked" (autoCompile_ == "true")
    _ -> pure unit

  gist <- getQueryStringMaybe "gist"
  case gist of
    Just gist_ -> JQuery.select "#view_gist" >>= JQuery.attr { href: "https://gist.github.com/" <> gist_ }
    Nothing -> JQuery.select "#view_gist_li" >>= JQuery.hide

  JQuery.select "input[name=backend_inputs]" >>= JQuery.on "change" \e jq -> do
    bc_@(BackendConfig newBackend) <- getBackendConfigFromView

    ok <- window >>= confirm ("Replace your current code with the " <> newBackend.backend <> " backend sample code?")
    if ok
      then navigateTo ("?backend=" <> newBackend.backend)
      else void $ setTimeout 1000 do
             compile bc_
             cacheCurrentCode
    hideMenus

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

main :: Eff ( alert :: ALERT
            , confirm :: CONFIRM
            , console :: CONSOLE
            , dom :: DOM
            , random :: RANDOM
            , timer :: TIMER
            ) Unit
main = JQuery.ready do
  JQuery.select "#showjs" >>= JQuery.on "change" \e _ ->
    getBackendConfigFromView >>= \bc -> compile bc

  JQuery.select "#compile_label" >>= JQuery.on "click" \e _ ->
    getBackendConfigFromView >>= \bc -> compile bc

  JQuery.select "input[name=view_mode]" >>= JQuery.on "change" \_ jq ->
    changeViewMode jq

  JQuery.select "#gist_save" >>= JQuery.on "click" \e _ -> publishNewGist

  JQuery.select "#hamburger" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#menu" >>= JQuery.toggleClass "show"

  JQuery.select "#view_mode_label" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#view_mode" >>= JQuery.toggleClass "show-sub-menu"

  JQuery.select "#backend_label" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#backend" >>= JQuery.toggleClass "show-sub-menu"

  JQuery.select "#editor_view" >>= JQuery.on "click" \e _ -> hideMenus

  createSessionIdIfNecessary withSession

module Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.JQuery (JQuery, addClass, append, appendText, attr, create, display, hide, on, ready, removeClass, select, setProp, setValue, toggleClass) as JQuery
import Control.Monad.Eff.JQuery.Extras (empty, filter, getValueMaybe, is) as JQuery
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn4, mkEffFn1, runEffFn1, runEffFn2, runEffFn4)
import Control.Monad.Except.Trans (runExceptT)
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
import Data.StrMap as StrMap
import Data.String (joinWith)
import Data.String as String
import Data.String.Regex (replace')
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Try.API (BackendConfig(..), CompileError(..), CompileResult(..), CompilerError(..), ErrorPosition(..), FailedResult(..), SuccessResult(..), getBackendConfigFromString)
import Try.API as API
import Try.Gist (getGistById, tryLoadFileFromGist, uploadGist)
import Try.QueryString (getQueryStringMaybe, setQueryStrings)
import Try.Session (createSessionIdIfNecessary, storeSession, tryRetrieveSession)
import Try.Types (JS(..))

-- | Display a loading message in the right hand column.
displayLoadingMessage :: forall eff. Eff (dom :: DOM | eff) Unit
displayLoadingMessage = do
  column2 <- JQuery.select "#column2"
  JQuery.empty column2
  div <- JQuery.create "<div>"
  JQuery.addClass "loading" div
  JQuery.appendText "Loading..." div
  JQuery.append div column2

-- | Display a list of errors in the right hand column.
displayErrors :: forall eff. Array CompilerError -> Eff (dom :: DOM | eff) Unit
displayErrors errs = do
  column2 <- JQuery.select "#column2"
  JQuery.empty column2
  forWithIndex_ errs \i (CompilerError{ message }) -> do
    h1 <- JQuery.create "<h1>"
    JQuery.addClass "error-banner" h1
    JQuery.appendText ("Error " <> show (i + 1) <> " of " <> show (Array.length errs)) h1

    pre <- JQuery.create "<pre>"
    code_ <- JQuery.create "<code>"
    JQuery.append code_ pre
    JQuery.appendText message code_

    JQuery.append h1 column2
    JQuery.append pre column2

-- | Display plain text in the right hand column.
displayPlainText
  :: forall eff
   . String
  -> Eff (dom :: DOM | eff) Unit
displayPlainText s = do
  column2 <- JQuery.select "#column2"
  JQuery.empty column2
  pre <- JQuery.create "<pre>"
  code_ <- JQuery.create "<code>"
  JQuery.append code_ pre
  JQuery.appendText s code_
  JQuery.append pre column2

isShowJsChecked :: forall eff. Eff (dom :: DOM | eff) Boolean
isShowJsChecked = JQuery.select "#showjs" >>= \jq -> JQuery.is jq ":checked"

isAutoCompileChecked :: forall eff. Eff (dom :: DOM | eff) Boolean
isAutoCompileChecked = JQuery.select "#auto_compile" >>= \jq -> JQuery.is jq ":checked"

-- | Hide the drop down menus
hideMenus :: forall eff. Eff (dom :: DOM | eff) Unit
hideMenus = do
  JQuery.select "#menu" >>= JQuery.removeClass "show"
  JQuery.select "#view_mode" >>= JQuery.removeClass "show-sub-menu"
  JQuery.select "#backend" >>= JQuery.removeClass "show-sub-menu"

-- | Update the view mode based on the menu selection
changeViewMode :: forall eff. Maybe String -> Eff (dom :: DOM | eff) Unit
changeViewMode viewMode = do
  column1 <- JQuery.select "#column1"
  column2 <- JQuery.select "#column2"
  showjs <- JQuery.select "#showjs"
  showjsLabel <- JQuery.select "#showjs_label"
  case viewMode of
    Just "code" -> do
      JQuery.display column1
      JQuery.hide column2
      JQuery.hide showjs
      JQuery.hide showjsLabel
    Just "output" -> do
      JQuery.hide column1
      JQuery.display column2
      JQuery.display showjs
      JQuery.display showjsLabel
    _ -> do
      JQuery.display column1
      JQuery.display column2
      JQuery.display showjs
      JQuery.display showjsLabel

getTextAreaContent :: forall eff. Eff (dom :: DOM | eff) String
getTextAreaContent = fold <$> (JQuery.select "#code_textarea" >>= JQuery.getValueMaybe)

setTextAreaContent :: forall eff. String -> Eff (dom :: DOM | eff) Unit
setTextAreaContent value = JQuery.select "#code_textarea" >>= JQuery.setValue value

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

-- | Compile the current code and execute it.
compile :: forall eff. BackendConfig -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
compile bc@(BackendConfig backend) = do
  code <- getTextAreaContent

  runContT (runExceptT (backend.compile code)) \res_ ->
    case res_ of
      Left err -> displayPlainText err
      Right res -> do
        cleanUpMarkers

        case res of
          Right (CompileSuccess (SuccessResult { js })) -> do
            showJs <- isShowJsChecked
            if showJs
              then displayPlainText js
              else runContT (runExceptT backend.getBundle)
                     case _ of
                       Left err -> error ("Unable to retrieve JS bundle: " <> err)
                       Right bundle -> execute (JS js) bundle bc
          Right (CompileFailed (FailedResult { error })) ->
            case error of
              CompilerErrors errs -> do
                displayErrors errs

                for_ errs \(CompilerError{ position }) ->
                  for_ (unwrap position) \(ErrorPosition pos) ->
                    runEffFn4 addErrorMarker
                      pos.startLine
                      pos.startColumn
                      pos.endLine
                      pos.endColumn
              OtherError err -> displayPlainText err
          Left errs -> do
            displayPlainText "Unable to parse the response from the server"
            traverse_ (error <<< renderForeignError) errs

-- | Execute the compiled code in a new iframe.
execute :: forall eff. JS -> JS -> BackendConfig -> Eff (dom :: DOM | eff) Unit
execute js bundle bc@(BackendConfig backend) = do
  let html = joinWith "\n"
        [ """<!DOCTYPE html>"""
        , """<html>"""
        , """  <head>"""
        , """    <meta content="text/html;charset=utf-8" http-equiv="Content-Type">"""
        , """    <meta content="utf-8" http-equiv="encoding">"""
        , """    <meta name="viewport" content="width=device-width, initial-scale=1.0">"""
        , """    <title>Try PureScript!</title>"""
        , """    <link rel="stylesheet" href="css/style.css">"""
        , backend.extra_styling
        , """  </head>"""
        , """  <body>"""
        , backend.extra_body
        , """  </body>"""
        , """</html>"""
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

  column2 <- JQuery.select "#column2"
  runEffFn4 setupIFrame column2 html scripts $ mkEffFn1 \body ->
    JQuery.on "click" (\_ _ -> hideMenus) body

-- | Setup the editor component and some event handlers.
setupEditor
  :: forall eff
   . { code :: String, backend :: BackendConfig }
  -> Eff ( alert :: ALERT
         , console :: CONSOLE
         , confirm :: CONFIRM
         , dom :: DOM
         , timer :: TIMER
         | eff
         ) Unit
setupEditor { code, backend } = do
  loadOptions backend
  setupBackendMenu backend

  setTextAreaContent code
  runEffFn1 setEditorContent code

  runEffFn2 onEditorChanged (mkEffFn1 \value -> do
    setTextAreaContent value
    cacheCurrentCode backend
    autoCompile <- isAutoCompileChecked
    when autoCompile do
      compile backend) 750

  JQuery.select "#showjs" >>= JQuery.on "change" \e _ ->
    compile backend

  JQuery.select "#compile_label" >>= JQuery.on "click" \e _ ->
    compile backend

  JQuery.select "#gist_save" >>= JQuery.on "click" \e _ ->
    publishNewGist backend

  compile backend
  cacheCurrentCode backend

loadFromGist
  :: forall eff
   . String
  -> BackendConfig
  -> ({ code :: String, backend :: BackendConfig }
      -> Eff ( confirm :: CONFIRM
             , console :: CONSOLE
             , dom :: DOM
             , timer :: TIMER
             | eff
             ) Unit)
  -> Eff ( confirm :: CONFIRM
         , console :: CONSOLE
         , dom :: DOM
         , timer :: TIMER
         | eff
         ) Unit
loadFromGist id_ backend k = do
  runContT (runExceptT (getGistById id_ >>= \gi -> tryLoadFileFromGist gi "Main.purs")) $
    case _ of
      Left err -> do
        error ("Unable to load gist metadata or contents: " <> err)
        k { code: "", backend }
      Right code -> k { code, backend }

withSession
  :: forall eff
   . String
  -> ({ code :: String, backend :: BackendConfig }
      -> Eff ( confirm :: CONFIRM
             , console :: CONSOLE
             , dom :: DOM
             , timer :: TIMER
             | eff
             ) Unit)
  -> Eff ( confirm :: CONFIRM
         , console :: CONSOLE
         , dom :: DOM
         , timer :: TIMER
         | eff
         ) Unit
withSession sessionId k = do
  state <- tryRetrieveSession sessionId
  case state of
    Just { code, backend } -> do
      k { code, backend: API.getBackendConfigFromString backend }
    Nothing -> do
      bc@(BackendConfig backend) <- API.getBackendConfigFromString <<< fromMaybe "core" <$> getQueryStringMaybe "backend"
      gist <- fromMaybe backend.mainGist <$> getQueryStringMaybe "gist"
      loadFromGist gist bc k

-- | Cache the current code in the session state
cacheCurrentCode
  :: forall eff
   . BackendConfig
  -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
cacheCurrentCode bc@(BackendConfig backend) = do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> do
      code <- getTextAreaContent
      storeSession sessionId_ { code, backend: backend.backend }
    Nothing -> error "No session ID"

-- | Create a new Gist using the current content
publishNewGist
  :: forall eff
   . BackendConfig
  -> Eff ( alert :: ALERT
         , confirm :: CONFIRM
         , console :: CONSOLE
         , dom :: DOM
         | eff
         ) Unit
publishNewGist bc@(BackendConfig backend) = do
  ok <- window >>= confirm (intercalate "\n"
          [ "Do you really want to publish this code as an anonymous Gist?"
          , ""
          , "Note: this code will be available to anyone with a link to the Gist."
          ])
  when ok do
    content <- getTextAreaContent
    runContT (runExceptT (uploadGist content)) $
      case _ of
        Left err -> do
          window >>= alert "Failed to create gist"
          error ("Failed to create gist: " <> err)
        Right gistId -> do
          setQueryStrings (StrMap.singleton "gist" gistId <>
                           StrMap.singleton "backend" backend.backend)

-- | Navigate to the specified URL.
navigateTo :: forall eff. String -> Eff (dom :: DOM | eff) Unit
navigateTo uri = void (window >>= location >>= setHref uri)

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
loadOptions bc = do
  viewMode <- getQueryStringMaybe "view"
  case viewMode of
    Just viewMode_
      | viewMode_ `elem` ["sidebyside", "code", "output"]
      -> changeViewMode viewMode
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

-- | Setup event listeners for the backend dropdown menu.
setupBackendMenu
  :: forall eff
   . BackendConfig
  -> Eff
       ( dom :: DOM
       , confirm :: CONFIRM
       , console :: CONSOLE
       , timer :: TIMER
       | eff
       )
       Unit
setupBackendMenu bc@(BackendConfig backend) = do
  JQuery.select ("#backend_" <> backend.backend) >>= JQuery.attr { checked: "checked" }
  JQuery.select "input[name=backend_inputs]" >>= JQuery.on "change" \e jq -> do
    bc_@(BackendConfig newBackend) <- getBackendConfigFromString <<< fromMaybe "core" <$> JQuery.getValueMaybe jq

    ok <- window >>= confirm ("Replace your current code with the " <> newBackend.backend <> " backend sample code?")
    if ok
      then navigateTo ("?backend=" <> newBackend.backend)
      else void $ setTimeout 1000 do
             compile bc_
             cacheCurrentCode bc_
    hideMenus

main :: Eff ( alert :: ALERT
            , confirm :: CONFIRM
            , console :: CONSOLE
            , dom :: DOM
            , random :: RANDOM
            , timer :: TIMER
            ) Unit
main = JQuery.ready do
  JQuery.select "input[name=view_mode]" >>= JQuery.on "change" \_ jq -> do
    viewMode <- JQuery.filter jq ":checked" >>= JQuery.getValueMaybe
    changeViewMode viewMode

  JQuery.select "#hamburger" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#menu" >>= JQuery.toggleClass "show"

  JQuery.select "#view_mode_label" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#view_mode" >>= JQuery.toggleClass "show-sub-menu"

  JQuery.select "#backend_label" >>= JQuery.on "click" \e _ -> do
    JQuery.select "#backend" >>= JQuery.toggleClass "show-sub-menu"

  JQuery.select "#editor_view" >>= JQuery.on "click" \e _ -> hideMenus

  runContT (do sessionId <- ContT createSessionIdIfNecessary
               ContT (withSession sessionId)) setupEditor

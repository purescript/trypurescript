module Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.JQuery (JQuery, addClass, append, appendText, attr, create, display, hide, on, ready, select, setProp, setValue) as JQuery
import Control.Monad.Eff.JQuery (setAttr)
import Control.Monad.Eff.JQuery.Extras (empty, filter, getValueMaybe, is) as JQuery
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn5, mkEffFn1, runEffFn1, runEffFn2, runEffFn3, runEffFn5)
import Control.Monad.Except.Trans (runExceptT)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (setHref)
import DOM.HTML.Types (ALERT, CONFIRM)
import DOM.HTML.Window (alert, confirm, location)
import Data.Array (mapMaybe)
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
import Try.API (BackendConfig(..), CompileError(..), CompileResult(..), CompileWarning(..), CompilerError(..), ErrorPosition(..), FailedResult(..), SuccessResult(..), getBackendConfigFromString)
import Try.API as API
import Try.Gist (getGistById, tryLoadFileFromGist, uploadGist)
import Try.QueryString (getQueryStringMaybe, setQueryStrings)
import Try.Session (createSessionIdIfNecessary, storeSession, tryRetrieveSession)
import Try.Types (JS(..))

displayLoadingMessage :: forall eff. Eff (dom :: DOM | eff) Unit
displayLoadingMessage = JQuery.select "#loading" >>= JQuery.display

hideLoadingMessage :: forall eff. Eff (dom :: DOM | eff) Unit
hideLoadingMessage = JQuery.select "#loading" >>= JQuery.hide

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

-- | Update the view mode based on the menu selection
changeViewMode :: forall eff. Maybe String -> Eff (dom :: DOM | eff) Unit
changeViewMode viewMode =
  for_ viewMode \viewMode_ ->
    JQuery.select "#editor_view" >>= setAttr "data-view-mode" viewMode_

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

-- | Add a visible marker at the specified location.
foreign import addMarker :: forall eff. EffFn5 (dom :: DOM | eff) String Int Int Int Int Unit

type Annotation =
  { row :: Int
  , column :: Int
  , type :: String
  , text :: String
  }

-- | Set the gutter annotations
foreign import setAnnotations :: forall eff. EffFn1 (dom :: DOM | eff) (Array Annotation) Unit

clearAnnotations :: forall eff. Eff (dom :: DOM | eff) Unit
clearAnnotations = runEffFn1 setAnnotations []

-- | Set up a fresh iframe in the specified container, and use it
-- | to execute the provided JavaScript code.
foreign import setupIFrame
  :: forall eff
   . EffFn3 (dom :: DOM | eff)
            JQuery.JQuery
            String
            String
            Unit

-- | Compile the current code and execute it.
compile :: forall eff. BackendConfig -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
compile bc@(BackendConfig backend) = do
  code <- getTextAreaContent

  displayLoadingMessage
  clearAnnotations

  runContT (runExceptT (backend.compile code)) \res_ ->
    case res_ of
      Left err -> displayPlainText err
      Right res -> do
        cleanUpMarkers

        case res of
          Right (CompileSuccess (SuccessResult { js, warnings })) -> do
            showJs <- isShowJsChecked
            if showJs
              then do hideLoadingMessage
                      displayPlainText js
              else runContT (runExceptT backend.getBundle) \bundleResult -> do
                     hideLoadingMessage
                     case bundleResult of
                       Left err -> error ("Unable to retrieve JS bundle: " <> err)
                       Right bundle -> do
                         for_ (unwrap warnings) \warnings_ -> do
                           let toAnnotation (CompileWarning{ errorCode, position, message }) =
                                 unwrap position <#> \(ErrorPosition pos) ->
                                   { row: pos.startLine - 1
                                   , column: pos.startColumn - 1
                                   , type: "warning"
                                   , text: message
                                   }
                           runEffFn1 setAnnotations (mapMaybe toAnnotation warnings_)
                         execute (JS js) bundle bc
          Right (CompileFailed (FailedResult { error })) -> do
            hideLoadingMessage
            case error of
              CompilerErrors errs -> do
                displayErrors errs

                let toAnnotation (CompilerError{ position, message }) =
                      unwrap position <#> \(ErrorPosition pos) ->
                        { row: pos.startLine - 1
                        , column: pos.startColumn - 1
                        , type: "error"
                        , text: message
                        }
                runEffFn1 setAnnotations (mapMaybe toAnnotation errs)

                for_ errs \(CompilerError{ position }) ->
                  for_ (unwrap position) \(ErrorPosition pos) ->
                    runEffFn5 addMarker
                      "error"
                      pos.startLine
                      pos.startColumn
                      pos.endLine
                      pos.endColumn
              OtherError err -> displayPlainText err
          Left errs -> do
            hideLoadingMessage
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
  runEffFn3 setupIFrame column2 html scripts

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
    Just gist_ -> JQuery.select ".view_gist" >>= JQuery.attr { href: "https://gist.github.com/" <> gist_ }
    Nothing -> JQuery.select ".view_gist_li" >>= JQuery.hide

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

  runContT (do sessionId <- ContT createSessionIdIfNecessary
               ContT (withSession sessionId)) setupEditor

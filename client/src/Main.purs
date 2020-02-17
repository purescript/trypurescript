module Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, fold, for_, intercalate, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String as String
import Data.String.Regex (replace')
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (error)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Foreign (renderForeignError)
import Foreign.Object as Object
import JQuery as JQuery
import JQuery.Extras as JQueryExtras
import Try.API (BackendConfig(..), CompileError(..), CompileResult(..), CompileWarning(..), CompilerError(..), ErrorPosition(..), FailedResult(..), SuccessResult(..), getBackendConfigFromString)
import Try.API as API
import Try.Gist (getGistById, tryLoadFileFromGist, uploadGist)
import Try.QueryString (getQueryStringMaybe, setQueryStrings)
import Try.Session (createSessionIdIfNecessary, storeSession, tryRetrieveSession)
import Try.Types (JS(..))
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (alert, confirm, location)

displayLoadingMessage :: Effect Unit
displayLoadingMessage = JQuery.select "#loading" >>= JQueryExtras.fadeIn

hideLoadingMessage :: Effect Unit
hideLoadingMessage = JQuery.select "#loading" >>= JQueryExtras.fadeOut

-- | Display a list of errors in the right hand column.
displayErrors :: Array CompilerError -> Effect Unit
displayErrors errs = do
  column2 <- JQuery.select "#column2"
  JQueryExtras.empty column2

  forWithIndex_ errs \i (CompilerError{ message }) -> do
    h1 <- JQuery.create "<h1>"
    JQuery.addClass "error-banner" h1
    JQuery.setText ("Error " <> show (i + 1) <> " of " <> show (Array.length errs)) h1

    pre <- JQuery.create "<pre>"
    code_ <- JQuery.create "<code>"
    JQuery.append code_ pre
    JQuery.setText message code_

    JQuery.append h1 column2
    JQuery.append pre column2

-- | Display plain text in the right hand column.
displayPlainText
  :: String
  -> Effect Unit
displayPlainText s = do
  column2 <- JQuery.select "#column2"
  JQueryExtras.empty column2
  pre <- JQuery.create "<pre>"
  code_ <- JQuery.create "<code>"
  JQuery.append code_ pre
  JQuery.setText s code_
  JQuery.append pre column2

isShowJsChecked :: Effect Boolean
isShowJsChecked = JQuery.select "#showjs" >>= \jq -> JQueryExtras.is jq ":checked"

isAutoCompileChecked :: Effect Boolean
isAutoCompileChecked = JQuery.select "#auto_compile" >>= \jq -> JQueryExtras.is jq ":checked"

-- | Update the view mode based on the menu selection
changeViewMode :: Maybe String -> Effect Unit
changeViewMode viewMode =
  for_ viewMode \viewMode_ ->
    JQuery.select "#editor_view" >>= JQuery.setAttr "data-view-mode" viewMode_

getTextAreaContent :: Effect String
getTextAreaContent = fold <$> (JQuery.select "#code_textarea" >>= JQueryExtras.getValueMaybe)

setTextAreaContent :: String -> Effect Unit
setTextAreaContent value = JQuery.select "#code_textarea" >>= JQuery.setValue value

-- | Set the editor content to the specified string.
foreign import setEditorContent :: EffectFn1 String Unit

-- | Register a callback for editor change events.
foreign import onEditorChanged
  :: EffectFn2 (EffectFn1 String Unit)
               Int
               Unit

-- | Clean up any global state associated with any visible error markers.
foreign import cleanUpMarkers :: Effect Unit

-- | Add a visible marker at the specified location.
foreign import addMarker :: EffectFn5 String Int Int Int Int Unit

type Annotation =
  { row :: Int
  , column :: Int
  , type :: String
  , text :: String
  }

-- | Set the gutter annotations
foreign import setAnnotations :: EffectFn1 (Array Annotation) Unit

clearAnnotations :: Effect Unit
clearAnnotations = runEffectFn1 setAnnotations []

-- | Set up a fresh iframe in the specified container, and use it
-- | to execute the provided JavaScript code.
foreign import setupIFrame
  :: EffectFn3 JQuery.JQuery
               String
               String
               Unit

-- | Compile the current code and execute it.
compile :: BackendConfig -> Effect Unit
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
                         for_ warnings \warnings_ -> do
                           let toAnnotation (CompileWarning{ errorCode, position, message }) =
                                 position <#> \(ErrorPosition pos) ->
                                   { row: pos.startLine - 1
                                   , column: pos.startColumn - 1
                                   , type: "warning"
                                   , text: message
                                   }
                           runEffectFn1 setAnnotations (mapMaybe toAnnotation warnings_)
                         execute (JS js) bundle bc
          Right (CompileFailed (FailedResult { error })) -> do
            hideLoadingMessage
            case error of
              CompilerErrors errs -> do
                displayErrors errs

                let toAnnotation (CompilerError{ position, message }) =
                      position <#> \(ErrorPosition pos) ->
                        { row: pos.startLine - 1
                        , column: pos.startColumn - 1
                        , type: "error"
                        , text: message
                        }
                runEffectFn1 setAnnotations (mapMaybe toAnnotation errs)

                for_ errs \(CompilerError{ position }) ->
                  for_ position \(ErrorPosition pos) ->
                    runEffectFn5 addMarker
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
execute :: JS -> JS -> BackendConfig -> Effect Unit
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
        "PS['" <> String.drop 12 (String.take (String.length s - 11) s) <> "']") (unwrap js)

      wrapped = joinWith "\n"
        [ "var module = {};"
        , "(function(module) {"
        , replaced
        , "})(module);"
        , "module.exports.main && module.exports.main();"
        ]

      scripts = joinWith "\n" [unwrap bundle, wrapped]

  column2 <- JQuery.select "#column2"
  runEffectFn3 setupIFrame column2 html scripts

-- | Setup the editor component and some event handlers.
setupEditor :: { code :: String, backend :: BackendConfig } -> Effect Unit
setupEditor { code, backend } = do
  loadOptions backend
  setupBackendMenu backend

  setTextAreaContent code
  runEffectFn1 setEditorContent code

  runEffectFn2 onEditorChanged (mkEffectFn1 \value -> do
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
  :: String
  -> BackendConfig
  -> ({ code :: String, backend :: BackendConfig } -> Effect Unit)
  -> Effect Unit
loadFromGist id_ backend k = do
  runContT (runExceptT (getGistById id_ >>= \gi -> tryLoadFileFromGist gi "Main.purs")) $
    case _ of
      Left err -> do
        window >>= alert err
        k { code: "", backend }
      Right code -> k { code, backend }

withSession
  :: String
  -> ({ code :: String, backend :: BackendConfig } -> Effect Unit)
  -> Effect Unit
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
cacheCurrentCode :: BackendConfig -> Effect Unit
cacheCurrentCode bc@(BackendConfig backend) = do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> do
      code <- getTextAreaContent
      storeSession sessionId_ { code, backend: backend.backend }
    Nothing -> error "No session ID"

-- | Create a new Gist using the current content
publishNewGist :: BackendConfig -> Effect Unit
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
          setQueryStrings (Object.singleton "gist" gistId <>
                           Object.singleton "backend" backend.backend)

-- | Navigate to the specified URL.
navigateTo :: String -> Effect Unit
navigateTo uri = void (window >>= location >>= setHref uri)

-- | Read query string options and update the state accordingly
loadOptions :: BackendConfig -> Effect Unit
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
setupBackendMenu :: BackendConfig -> Effect Unit
setupBackendMenu bc@(BackendConfig backend) = do
  JQuery.select ("#backend_" <> backend.backend) >>= JQuery.attr { checked: "checked" }
  JQuery.select "input[name=backend_inputs]" >>= JQuery.on "change" \e jq -> do
    bc_@(BackendConfig newBackend) <- getBackendConfigFromString <<< fromMaybe "core" <$> JQueryExtras.getValueMaybe jq

    ok <- window >>= confirm ("Replace your current code with the " <> newBackend.backend <> " backend sample code?")
    if ok
      then navigateTo ("?backend=" <> newBackend.backend)
      else void $ setTimeout 1000 do
             compile bc_
             cacheCurrentCode bc_

main :: Effect Unit
main = JQuery.ready do
  JQuery.select "input[name=view_mode]" >>= JQuery.on "change" \_ jq -> do
    viewMode <- JQueryExtras.filter jq ":checked" >>= JQueryExtras.getValueMaybe
    changeViewMode viewMode

  runContT (do sessionId <- ContT createSessionIdIfNecessary
               ContT (withSession sessionId)) setupEditor

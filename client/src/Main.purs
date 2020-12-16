module Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, fold, for_, intercalate, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn5, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn5)
import Foreign (renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import JQuery as JQuery
import JQuery.Extras as JQueryExtras
import Try.API (CompileError(..), CompileResult(..), CompileWarning(..), CompilerError(..), ErrorPosition(..), FailedResult(..), SuccessResult(..))
import Try.API as API
import Try.Config as Config
import Try.Gist (getGistById, tryLoadFileFromGist, uploadGist)
import Try.Loader (Loader, makeLoader, runLoader)
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
  :: EffectFn2 JQuery.JQuery
               (Object JS)
               Unit

loader :: Loader
loader = makeLoader Config.loaderUrl

-- | Compile the current code and execute it.
compile :: Effect Unit
compile = do
  code <- getTextAreaContent

  displayLoadingMessage
  clearAnnotations

  launchAff_ $ runExceptT (API.compile Config.compileUrl code) >>= \res_ ->
    case res_ of
      Left err -> liftEffect $ displayPlainText err
      Right res -> do
        liftEffect cleanUpMarkers

        case res of
          Right (CompileSuccess (SuccessResult { js, warnings })) -> do
            showJs <- liftEffect isShowJsChecked
            if showJs then liftEffect do
              hideLoadingMessage
              displayPlainText js
            else do
              sources <- runExceptT $ runLoader loader (JS js)
              liftEffect hideLoadingMessage
              for_ warnings \warnings_ -> liftEffect do
                let toAnnotation (CompileWarning{ errorCode, position, message }) =
                      position <#> \(ErrorPosition pos) ->
                        { row: pos.startLine - 1
                        , column: pos.startColumn - 1
                        , type: "warning"
                        , text: message
                        }
                runEffectFn1 setAnnotations (mapMaybe toAnnotation warnings_)
              for_ sources (liftEffect <<< execute (JS js))
          Right (CompileFailed (FailedResult { error })) -> liftEffect do
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
          Left errs -> liftEffect do
            hideLoadingMessage
            displayPlainText "Unable to parse the response from the server"
            traverse_ (error <<< renderForeignError) errs

-- | Execute the compiled code in a new iframe.
execute :: JS -> Object JS -> Effect Unit
execute js modules = do
  let eventData = Object.insert "<file>" js modules
  column2 <- JQuery.select "#column2"
  runEffectFn2 setupIFrame column2 eventData

-- | Setup the editor component and some event handlers.
setupEditor :: forall r. { code :: String | r } -> Effect Unit
setupEditor { code } = do
  loadOptions
  setTextAreaContent code
  runEffectFn1 setEditorContent code

  runEffectFn2 onEditorChanged (mkEffectFn1 \value -> do
    setTextAreaContent value
    cacheCurrentCode
    autoCompile <- isAutoCompileChecked
    when autoCompile do
      compile) 750

  JQuery.select "#showjs" >>= JQuery.on "change" \e _ ->
    compile

  JQuery.select "#compile_label" >>= JQuery.on "click" \e _ ->
    compile

  JQuery.select "#gist_save" >>= JQuery.on "click" \e _ ->
    launchAff_ publishNewGist

  compile
  cacheCurrentCode

loadFromGist
  :: String
  -> Aff { code :: String }
loadFromGist id = do
  runExceptT (getGistById id >>= \gi -> tryLoadFileFromGist gi "Main.purs") >>= case _ of
    Left err -> do
      liftEffect $ window >>= alert err
      pure { code: "" }
    Right code ->
      pure { code }

withSession
  :: String
  -> Aff { code :: String }
withSession sessionId = do
  state <- liftEffect $ tryRetrieveSession sessionId
  case state of
    Just state' -> pure state'
    Nothing -> do
      gist <- liftEffect $ fromMaybe Config.mainGist <$> getQueryStringMaybe "gist"
      loadFromGist gist

-- | Cache the current code in the session state
cacheCurrentCode :: Effect Unit
cacheCurrentCode  = do
  sessionId <- getQueryStringMaybe "session"
  case sessionId of
    Just sessionId_ -> do
      code <- getTextAreaContent
      storeSession sessionId_ { code }
    Nothing -> error "No session ID"

-- | Create a new Gist using the current content
publishNewGist :: Aff Unit
publishNewGist = do
  ok <- liftEffect $ window >>= confirm (intercalate "\n"
          [ "Do you really want to publish this code as an anonymous Gist?"
          , ""
          , "Note: this code will be available to anyone with a link to the Gist."
          ])
  when ok do
    content <- liftEffect $ getTextAreaContent
    runExceptT (uploadGist content) >>= case _ of
      Left err -> liftEffect do
        window >>= alert "Failed to create gist"
        error ("Failed to create gist: " <> err)
      Right gistId -> liftEffect do
        setQueryStrings (Object.singleton "gist" gistId)

-- | Navigate to the specified URL.
navigateTo :: String -> Effect Unit
navigateTo uri = void (window >>= location >>= setHref uri)

-- | Read query string options and update the state accordingly
loadOptions :: Effect Unit
loadOptions = do
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

main :: Effect Unit
main = JQuery.ready do
  JQuery.select "input[name=view_mode]" >>= JQuery.on "change" \_ jq -> do
    viewMode <- JQueryExtras.filter jq ":checked" >>= JQueryExtras.getValueMaybe
    changeViewMode viewMode

  createSessionIdIfNecessary \sessionId -> launchAff_ do
    code <- withSession sessionId
    liftEffect $ setupEditor code

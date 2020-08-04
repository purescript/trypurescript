module Try.Component where

import Prelude
import Ace (Document, EditSession, Range, ace, edit)
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Marker as Marker
import Data.Argonaut (encodeJson, stringify)
import Data.Array (catMaybes, concat, insertAt, mapMaybe, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (multiline)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as HK
import Halogen.Hooks.Extra.Hooks (useDebouncer, useModifyState_, usePutState)
import Halogen.Query.EventSource as ES
import LzString (compressToEncodedURIComponent)
import MyAce as MyAce
import Tailwind as T
import Try.API (AnnotationType(..), CompileError(..), CompileResult(..), CompilerError, WarningOrError, compile, mkMarkerRange, toAnnotation)
import Try.Classes (Responsiveness(..), commonMenuClasses, dropdownItemClasses, menuTextClasses, nonMobileBlock, nonMobileBlockClasses)
import Try.Common (Content(..), GhPath(..), GhToken, GistID(..), gistQP, homeRoute, pursQP)
import Try.Gist (getFile, ghAuthorize, ghCreateGist, ghGetGist, ghRequestToken)
import Try.Loader (Loader, makeLoader, runLoader)
import Try.Routing (Route(..))
import Try.Types (JS(..))
import Try.Utility (ContentSource(..), PushRoute, ViewMode(..), compress, decompress)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

{-
The main component for this app
-}
--
type Input
  = PushRoute

-- Query must be (Type -> Type)
data Query a
  = Nav Route a

-- This is required for good caching performance
loader :: Loader
loader = makeLoader

-- What can be shown in the output pane
data Output
  = Text String -- startup or simple error
  | Loading -- loading gif
  | App String JS -- json string and main JS
  | Errors (Array CompilerError) -- compiler errors

component :: forall o m. MonadAff m => H.Component HH.HTML Query Input o m
component =
  -- No tokens for child slots or output
  HK.component \({ queryToken } :: HK.ComponentTokens Query _ o) input -> HK.do
    -- Annotations for initial values not required, but helps with clarity
    viewMode /\ putViewMode <- usePutState (SideBySide :: ViewMode)
    autoCompile /\ autoCompileIdx <- useState true
    showJS /\ modifyShowJS <- useModifyState_ false
    dropdownOpen /\ putDropdownOpen <- usePutState false
    content /\ contentIdx <- useState $ Content ""
    document /\ putDocument <- usePutState (Nothing :: Maybe Document)
    session /\ sessionIdx <- useState (Nothing :: Maybe EditSession)
    route /\ putRoute <- usePutState (Nothing :: Maybe Route)
    ghToken /\ putGhToken <- usePutState (Nothing :: Maybe GhToken)
    pushRoute /\ putPushRoute <- usePutState (input :: PushRoute)
    contentSource /\ putContentSource <- usePutState (NewContent :: ContentSource)
    output /\ putOutput <- usePutState $ (Text "" :: Output)
    --
    -- Helper functions to reduce code duplication
    let
      -- GhToken is a parameter rather than picked-up from state
      -- to ensure it is not Nothing
      doSaveGist gh_token = do
        -- Cannot just use `content` - will be stale
        currentContent <- HK.get contentIdx
        log $ "saving gist, content: " <> show currentContent
        eitherId <- liftAff $ ghCreateGist gh_token $ currentContent
        case eitherId of
          Left err -> log err
          Right id -> do
            putContentSource $ HaveGist id
            liftEffect $ pushRoute $ "/?" <> gistQP <> "=" <> (show id)

      -- Create editor annotations and markers for errors or warnings
      annotateAndMark :: forall r. AnnotationType -> Array (WarningOrError r) -> _
      annotateAndMark type_ arr = do
        ses <- HK.get sessionIdx
        case ses of
          Nothing -> do
            log "Error: Session unset"
          Just s ->
            liftEffect do
              -- annotations
              let
                annotations = mapMaybe (toAnnotation type_) arr
              Session.setAnnotations annotations s
              -- markers
              maybeRanges <- traverse mkMarkerRange arr
              let
                (ranges :: Array Range) = catMaybes maybeRanges
              -- The shown type of "error" or "warning" should match the css class
              -- found in ace.css.
              -- `true` to show in "front"
              traverse_ (\r -> Session.addMarker r (show type_) "text" true s) ranges

      -- Clear annotations and marks from editor
      removeAnnotationsAndMarks = do
        ses <- HK.get sessionIdx
        case ses of
          Nothing -> log "Error: Session unset"
          Just s ->
            liftEffect do
              -- Remove annotations
              Session.clearAnnotations s
              -- A `removeMarkers` function would be nice...
              -- Get all inFront(=true) markers
              markers <- MyAce.getMarkers true s
              (markerIds :: Array Int) <- traverse Marker.getId markers
              -- Remove markers
              traverse_ (\m -> Session.removeMarker m s) markerIds

      doCompile = do
        -- Needed now that this is called from debouncer.
        -- Stale otherwise.
        currentContent <- HK.get contentIdx
        -- Clear annotations and marks from editor
        removeAnnotationsAndMarks
        putOutput Loading
        (res :: Either String CompileResult) <- liftAff $ compile currentContent
        case res of
          Right (CompileSuccess { js, warnings }) -> do
            -- warnings is maybe array. Why not just let this be an array?
            -- Create annotations and markers for warnings
            annotateAndMark AnnotateWarning $ fromMaybe [] warnings
            -- Get JS source files for all included modules
            obj <- liftAff $ runLoader loader $ JS js
            -- Compress source file contents.
            -- This avoids non-trivial HTML + JSON string escaping
            let
              objcomp = map (unwrap >>> compressToEncodedURIComponent) obj
            -- Convert object to JSON string
            let
              jsonStr = stringify $ encodeJson objcomp
            -- Pass JS files to execute in iframe
            putOutput $ App jsonStr $ JS js
          Right (CompileFailed { error }) -> do
            case error of
              CompilerErrors (errs :: Array CompilerError) -> do
                putOutput $ Errors errs
                -- Create annotations and markers for errors
                annotateAndMark AnnotateError errs
              OtherError err -> do
                putOutput $ Text err
          Left err -> do
            let
              str = "bad result. Likely communication issue with compiler: " <> err
            putOutput $ Text str
    --
    -- Debouncer for auto-compile
    debouncedRecompile <-
      useDebouncer (Milliseconds 1000.0) \_ -> do
        -- Recompile if setting is enabled
        autoRecomp <- HK.get autoCompileIdx
        if autoRecomp then do
          doCompile
        else
          pure unit -- don't recompile
    --
    -- Must put writeContent after debouncedRecompile
    let
      -- update content in editor and state
      writeContent (Content ct) = do
        -- Automatically modify non-Main module names
        let
          ctEdit = case regex "^module (?!Main ).*" multiline of
            Left err -> "-- Error constructing regex: " <> err <> "\n" <> ct
            Right reg -> replace reg "module Main where -- Overwritten by Try PureScript" ct
        --log $ "writing content: " <> ctEdit
        HK.put contentIdx $ Content ctEdit
        liftEffect $ traverse_ (Document.setValue ctEdit) document
        -- Queue an automatic recompile (ignored if setting disabled)
        debouncedRecompile unit
    --
    -- Initialize Ace editor and subscribe to text changes
    HK.useLifecycleEffect do
      doc /\ ses <-
        liftEffect do
          -- Create an editor
          editor <- edit "editor" ace
          -- disable vertical line on right side of editor that
          -- estimates where words will be wrapped on paper.
          Editor.setShowPrintMargin false editor
          session_ <- Editor.getSession editor
          -- Haskell syntax highlighting.
          -- We could build more specialized highlighting for purs:
          -- https://ace.c9.io/#higlighter=&nav=higlighter
          Session.setMode "ace/mode/haskell" session_
          Session.setTabSize 2 session_
          document_ <- Session.getDocument session_
          pure $ document_ /\ session_
      --
      -- Handle changes within editor.
      -- Ignoring returned subscription ID.
      _ <-
        HK.subscribe do
          ES.effectEventSource \emitter -> do
            -- Ignoring DocumentEvent
            Document.onChange doc \_ -> do
              str <- Document.getValue doc
              let
                newContent = Content str
              ES.emit emitter do
                -- Compare content to prevent clearing gist status immediately upon gist load
                oldContent <- HK.get contentIdx
                if (newContent /= oldContent) then do
                  -- New content clears existing contentSource
                  putContentSource NewContent
                  writeContent newContent
                  liftEffect $ pushRoute $ "/?" <> pursQP <> "=" <> (show $ compress newContent)
                else do
                  -- Do nothing if content unchanged
                  pure unit
            -- No finalizer, so return mempty
            pure mempty
      putDocument $ Just doc
      HK.put sessionIdx $ Just ses
      pure Nothing
    --
    -- Handle routing queries
    HK.useQuery queryToken \(Nav rt a) -> do
      -- multiple state modifications, but not a performance issue now.
      putRoute $ Just rt
      case rt of
        AuthorizeCallback authCode compressed -> do
          log "in auth callback"
          -- Immediately show new content.
          -- This also requires setting saving flag again, since state
          -- is reset upon page refresh from callback.
          writeContent $ decompress compressed
          putContentSource SavingGist
          -- Make ghToken request to private app server
          res <- liftAff $ ghRequestToken authCode
          case res of
            Left err -> log err
            Right gh_token -> do
              -- Save ghToken
              putGhToken $ Just gh_token
              -- Save gist
              doSaveGist gh_token
        LoadCompressed compressed -> do
          let
            ct = decompress compressed
          --log $ "Got content from url: " <> show ct
          writeContent ct
        LoadGist gist_id -> do
          eitherContent <- liftAff $ ghGetGist gist_id
          case eitherContent of
            Left err -> putOutput $ Text $ "Failed to load gist at: https://gist.github.com/" <> show gist_id <> "\n" <> err
            Right c -> do
              --log $ "Got content from gist: " <> show c
              writeContent c
              putContentSource $ HaveGist gist_id
        LoadGitHub (GhPath path) -> do
          let
            rawPath = "https://raw.githubusercontent.com/" <> path

            elements = split (Pattern "/") path

            richPath =
              "https://github.com/"
                <> case insertAt 2 "blob" elements of
                    Just arr -> joinWith "/" arr
                    Nothing -> "problem_with_original_github_path"
          eitherContent <- liftAff $ getFile rawPath
          case eitherContent of
            Left err -> putOutput $ Text err
            Right c -> do
              --log $ "Got content from gh: " <> show c
              writeContent c
              putContentSource $ HaveGhFile richPath
        --
        -- Note that pushRoute just sets the URL, but won't reload the page:
        -- liftEffect $ pushRoute homeRoute
        Home -> liftEffect $ window >>= location >>= setHref homeRoute
      -- Required response boilerplate for query
      pure (Just a)
    --
    -- Helper functions for rendering
    let
      menu =
        HH.div
          [ HP.classes
              [ T.flex
              , T.bgTpsBlack
              ]
          ]
          [ HH.a
              [ HP.href homeRoute
              , HP.title "Try PureScript!"
              , HP.classes commonMenuClasses
              ]
              -- Could also define image width/height in css
              [ HH.img
                  [ HP.src $ "img/favicon-white.svg"
                  , HP.width 40
                  , HP.width 40
                  ]
              ]
          , viewModeDropdown
          , mkClickButton
              "Compile"
              "Compile Now"
              NonMobile
              doCompile
          , mkToggleButton
              "Auto-Compile"
              "Compile on code changes"
              autoCompile
              NonMobile
              ( do
                  -- toggle setting
                  HK.modify_ autoCompileIdx not
                  -- Queue a recompile in the case of enabling.
                  -- Will be ignored if setting is disabled.
                  debouncedRecompile unit
              )
          , mkToggleButton
              "Show JS"
              "Show resulting JavaScript code instead of output"
              showJS
              RenderAlways
              $ modifyShowJS not
          , gistButtonOrLink
          , newTabLink
              "Help"
              "Learn more about Try PureScript"
              "https://github.com/purescript/trypurescript/blob/master/README.md"
          ]

      mkClickButton text title res action =
        HH.button
          [ HP.classes $ menuTextClasses <> nonMobileBlock res
          , HP.title title
          , HE.onClick \_ -> Just action
          ]
          [ HH.text text ]

      mkToggleButton text title enabled res action =
        HH.button
          [ HP.classes $ menuTextClasses <> highlight <> nonMobileBlock res
          , HP.title title
          , HE.onClick \_ -> Just action
          ]
          [ HH.text text ]
        where
        highlight = if enabled then [ T.textTpsEnabled ] else [ T.textTpsDisabled, T.lineThrough ]

      viewModeDropdown =
        let
          dropdownItems =
            if dropdownOpen then
              HH.div
                [ HP.classes
                    [ T.absolute
                    , T.z10
                    , T.wFull
                    ]
                ]
                $ map
                    mkDropdownItem
                    [ SideBySide, Code, Output ]
            else
              HH.div_ []
        in
          HH.div
            [ HP.classes [ T.relative ]
            , HE.onMouseEnter \_ -> Just $ putDropdownOpen true
            , HE.onMouseLeave \_ -> Just $ putDropdownOpen false
            ]
            [ HH.div
                [ HP.classes $ menuTextClasses <> nonMobileBlockClasses
                , HP.title "Select a view mode"
                ]
                [ HH.text "View Mode ▾" ]
            , dropdownItems
            ]

      mkDropdownItem vm =
        HH.button
          [ HP.classes $ dropdownItemClasses <> highlight
          , HE.onClick \_ -> Just $ putViewMode vm
          ]
          [ HH.text $ show vm ]
        where
        highlight = if vm == viewMode then [ T.textTpsEnabled ] else []

      newTabLink text title href =
        HH.a
          [ HP.href href
          , HP.target "_blank" -- Open in new tab
          , HP.classes menuTextClasses
          , HP.title title
          ]
          [ HH.text text ]

      gistButtonOrLink = case contentSource of
        NewContent ->
          mkClickButton
            "Save Gist"
            "Save code to GitHub Gist (requires OAuth login)"
            NonMobile
            ( do
                -- Immediately show "saving" status
                putContentSource SavingGist
                case ghToken of
                  Nothing -> do
                    log "need token - authorizing"
                    liftEffect $ ghAuthorize content
                  Just gh_token -> do
                    log "have token"
                    doSaveGist gh_token
            )
        SavingGist ->
          HH.div
            [ HP.classes menuTextClasses ]
            [ HH.text "Saving..." ]
        HaveGist (GistID id) ->
          newTabLink
            "View Gist"
            "Open the original gist in a new window"
            $ "https://gist.github.com/"
            <> id
        HaveGhFile url ->
          newTabLink
            "View File"
            "Open the original GitHub file in a new window"
            url

      renderIframe jsonStr =
        HH.iframe
          [ sandbox "allow-scripts allow-top-navigation"
          , HP.src "frame-error.html"
          , srcDoc
              $ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js\"></script>"
              <> "<script src='frame.js'> loadFrame('"
              <> jsonStr
              <> "'); </script>"
          , HP.classes [ T.flexGrow ]
          ]

      renderOutputInner = case output of
        Text str -> [ HH.text str ]
        Loading ->
          [ HH.img
              [ HP.src "img/loading.gif"
              , HP.classes [ T.objectContain ]
              ]
          ]
        App str (JS js) ->
          if showJS then
            [ HH.pre [ HP.classes [ T.textXs ] ] [ HH.code_ [ HH.text js ] ] ]
          else
            [ renderIframe str ]
        Errors errs -> concat $ mapWithIndex mkError errs
          where
          len = show $ Array.length errs

          mkError i { message } =
            [ HH.code
                [ HP.classes [ T.fontBold, T.text2xl, T.py2, T.px5 ] ]
                [ HH.text $ "Error " <> show (i + 1) <> " of " <> len ]
            , HH.pre_ [ HH.code_ [ HH.text message ] ]
            ]

      renderOutput =
        HH.div
          [ HP.classes $ [ T.flex, T.flexCol, T.flexGrow, T.w0 ] <> hidden ]
          renderOutputInner
        where
        hidden = case viewMode of
          Code -> [ T.hidden ]
          _ -> [] -- don't hide for output or side-by-side

      renderEditor =
        HH.div
          [ HP.classes $ [ T.hidden ] <> display
          ]
          [ HH.div
              [ HP.classes [ T.flexGrow ]
              , HP.id_ "editor"
              ]
              []
          ]
        where
        display = case viewMode of
          -- don't show in output-only view mode
          Output -> []
          -- display for code or side-by-side
          _ -> [ T.smFlex, T.flexGrow, T.borderR4, T.borderGray100 ]

      banner =
        HH.div
          [ HP.classes [ T.block, T.smHidden, T.bgTpsMobileBanner, T.textTpsBlack, T.textSm, T.borderB, T.borderTpsBlack, T.p1, T.mb2 ] ]
          [ HH.text "Your screen size is too small. Code editing has been disabled." ]
    --
    -- Render
    HK.pure do
      HH.div [ HP.classes [ T.flex, T.flexCol, T.hScreen ] ]
        [ menu
        , banner
        , HH.div
            [ HP.classes [ T.flex, T.flexRow, T.flexGrow, T.spaceX1 ] ]
            [ renderEditor, renderOutput ]
        ]

{-
Todo create Halogen PRs for these missing props
-}
sandbox :: forall r i. String -> HH.IProp ( sandbox :: String | r ) i
sandbox = HH.prop (HH.PropName "sandbox")

srcDoc :: forall r i. String -> HH.IProp ( srcDoc :: String | r ) i
srcDoc = HH.prop (HH.PropName "srcdoc")

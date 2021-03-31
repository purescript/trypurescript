module Try.Editor
  ( Slot
  , Query(..)
  , MarkerType(..)
  , toStringMarkerType
  , Output(..)
  , component
  ) where

import Prelude

import Ace (Annotation, Range)
import Ace as Ace
import Ace.EditSession as EditSession
import Ace.Editor as Edit
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types (Editor)
import Ace.VirtualRenderer as Renderer
import Data.Foldable (for_, traverse_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Fiber, forkAff)
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Try.API (ErrorPosition)
import Web.HTML (HTMLElement)

type Slot = H.Slot Query Output

data Query a
  = GetEditorContent (Maybe String -> a)
  | SetEditorContent String a
  | SetAnnotations (Array Annotation) a
  | AddMarker MarkerType ErrorPosition a
  | RemoveMarkers a

data Output = TextChanged String

data MarkerType = MarkerError | MarkerWarning

toStringMarkerType :: MarkerType -> String
toStringMarkerType = case _ of
  MarkerError -> "error"
  MarkerWarning -> "warning"

newtype MarkerId = MarkerId Int
derive instance newtypeMarkerId :: Newtype MarkerId _

derive instance eqMarkerType :: Eq MarkerType

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

debounceTime :: Milliseconds
debounceTime = Milliseconds 750.0

type State =
  { editor :: Maybe Editor
  , markers :: List MarkerId
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  }

data Action
  = Initialize
  | Finalize
  | ClearMarkers
  | HandleChange

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }
  where
  initialState :: i -> State
  initialState _ =
    { editor: Nothing
    , markers: Nil
    , debounceRef: Nothing
    }

  -- As we're embedding a 3rd party component we only need to create a placeholder
  -- div here and attach the ref property which will let us reference the element.
  render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.div
      [ HP.ref $ H.RefLabel "ace"
      , HP.id_ "code"
      ]
      [ ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Initialize -> do
      H.getHTMLElementRef (H.RefLabel "ace") >>= traverse_ \element -> do
        debounceRef <- H.liftEffect $ Ref.new Nothing
        editor <- H.liftEffect $ setupEditor element
        H.modify_ _ { editor = Just editor, debounceRef = Just debounceRef }
        session <- H.liftEffect $ Editor.getSession editor
        void $ H.subscribe $ ES.effectEventSource \emitter -> do
          EditSession.onChange session (\_ -> ES.emit emitter HandleChange)
          pure mempty

    Finalize -> do
      handleAction ClearMarkers
      H.modify_ _ { editor = Nothing }

    ClearMarkers -> do
      { editor: mbEditor, markers } <- H.get
      for_ mbEditor \editor -> H.liftEffect do
        session <- Editor.getSession editor
        let cleanup (MarkerId n) = EditSession.removeMarker n session
        traverse_ cleanup markers

    HandleChange -> do
      let
        handleChange = H.gets _.editor >>= traverse_ \editor -> do
          text <- H.liftEffect (Editor.getValue editor)
          H.raise $ TextChanged text

      H.gets _.debounceRef >>= traverse_ \ref -> do
        mbDebouncer <- H.liftEffect $ Ref.read ref
        case mbDebouncer of
          Nothing -> do
            var   <- H.liftAff AVar.empty
            fiber <- H.liftAff $ forkAff do
              Aff.delay debounceTime
              AVar.put unit var

            -- This compututation will fork and run in the background. When the
            -- var is finally filled, the action will run.
            _ <- H.fork do
              H.liftAff $ AVar.take var
              H.liftEffect $ Ref.write Nothing ref
              handleChange

            H.liftEffect $ Ref.write (Just { var, fiber }) ref

          Just debouncer -> do
            H.liftAff $ Aff.killFiber (Aff.error "Time's up!") debouncer.fiber
            fiber <- H.liftAff $ forkAff do
              Aff.delay debounceTime
              AVar.put unit debouncer.var
            H.liftEffect $ Ref.write (Just (debouncer { fiber = fiber })) ref

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    GetEditorContent reply -> do
      contents <- H.gets _.editor >>= traverse (Editor.getValue >>> H.liftEffect)
      pure (Just (reply contents))

    SetEditorContent text next -> do
      H.gets _.editor >>= traverse_ \editor -> H.liftEffect do
        current <- Editor.getValue editor
        when (text /= current) do
          session <- Edit.getSession editor
          EditSession.setValue text session
      pure (Just next)

    SetAnnotations annotations next -> do
      H.gets _.editor >>= traverse_ \editor -> H.liftEffect do
        session <- Editor.getSession editor
        EditSession.setAnnotations annotations session
      pure (Just next)

    AddMarker markerType position next -> do
      H.gets _.editor >>= traverse_ \editor -> do
        markerId <- H.liftEffect do
          session <- Editor.getSession editor
          range <- rangeFromPosition position
          let mt = toStringMarkerType markerType
          EditSession.addMarker range mt "text" true session
        H.modify_ \st -> st { markers = Cons (MarkerId markerId) st.markers }
      pure (Just next)

    RemoveMarkers next -> do
      handleAction ClearMarkers
      pure (Just next)

setupEditor :: HTMLElement -> Effect Editor
setupEditor element = do
  editor <- Ace.editNode element Ace.ace
  Editor.setShowPrintMargin false editor
  Editor.setTheme "ace/theme/dawn" editor

  renderer <- Editor.getRenderer editor
  Renderer.setShowGutter true renderer

  session <- H.liftEffect $ Editor.getSession editor
  EditSession.setMode "ace/mode/haskell" session
  EditSession.setTabSize 2 session
  EditSession.setUseSoftTabs true session

  pure editor

rangeFromPosition :: ErrorPosition -> Effect Range
rangeFromPosition pos = do
  let
    -- Ensure ranges are at least one character wide
    { startLine, startColumn, endLine, endColumn } =
      if pos.startLine == pos.endLine && pos.endColumn <= pos.startColumn then
        if pos.startColumn > 0 then do
          pos { startColumn = pos.endColumn - 1 }
        else
          pos { endColumn = pos.startColumn + 1 }
      else
        pos

  Range.create
    (startLine - 1)
    (startColumn - 1)
    (endLine - 1)
    (endColumn - 1)

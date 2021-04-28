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
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
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

debounceTime :: Milliseconds
debounceTime = Milliseconds 750.0

type State =
  { editor :: Maybe Editor
  , markers :: List MarkerId
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
        editor <- H.liftEffect $ setupEditor element
        H.modify_ _ { editor = Just editor }
        session <- H.liftEffect $ Editor.getSession editor
        void $ H.subscribe $ ES.effectEventSource \emitter -> do
          emit <- debounce debounceTime \_ -> ES.emit emitter HandleChange
          EditSession.onChange session emit
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
      H.gets _.editor >>= traverse_ \editor -> do
        text <- H.liftEffect (Editor.getValue editor)
        H.raise $ TextChanged text

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

debounce :: forall a. Milliseconds -> (a -> Effect Unit) -> Effect (a -> Effect Unit)
debounce (Milliseconds wait) k = do
  tidRef <- Ref.new Nothing
  pure \a -> do
    Ref.read tidRef >>= traverse_ clearTimeout
    tid <- setTimeout (Int.floor wait) do
      Ref.write Nothing tidRef
      k a
    Ref.write (Just tid) tidRef

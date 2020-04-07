module TryPureScript
  ( Inline
  , Doc
  , text
  , p
  , link
  , code
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , list
  , indent
  , render
  , withConsole
  ) where

import Prelude
import Data.Foldable (class Foldable, foldMap)
import Data.String (joinWith)
import Effect (Effect)

foreign import setInnerHTML :: String -> Effect Unit

foreign import encode :: String -> String

foreign import withConsoleImpl
  :: forall a
   . Effect a
  -> Effect (Array String)

withConsole
  :: forall a
   . Effect a
  -> Effect Doc
withConsole f = map toDoc (withConsoleImpl f) where
  toDoc = Doc <<< tag' "pre" <<< tag' "code" <<< joinWith "\n"

tag :: String -> String -> String -> String
tag open close html = "<" <> open <> ">" <> html <> "</" <> close <> ">"

tag' :: String -> String -> String
tag' open = tag open open

newtype Inline = Inline String

unInline :: Inline -> String
unInline (Inline html) = html

derive newtype instance semigroupInline :: Semigroup Inline
derive newtype instance monoidInline :: Monoid Inline

text :: String -> Inline
text = Inline <<< encode

link :: String -> Inline -> Inline
link url (Inline html) = Inline (tag ("a href=\"" <> encode url <> "\" target=\"_top\"") "a" html)

code :: Inline -> Inline
code (Inline s) = Inline (tag' "code" s)

newtype Doc = Doc String

unDoc :: Doc -> String
unDoc (Doc html) = html

render :: Doc -> Effect Unit
render (Doc html) = setInnerHTML html

derive newtype instance semigroupDoc :: Semigroup Doc
derive newtype instance monoidDoc :: Monoid Doc

p :: Inline -> Doc
p (Inline s) = Doc (tag' "p" s)

h1 :: Inline -> Doc
h1 (Inline s) = Doc (tag' "h1" s)

h2 :: Inline -> Doc
h2 (Inline s) = Doc (tag' "h2" s)

h3 :: Inline -> Doc
h3 (Inline s) = Doc (tag' "h3" s)

h4 :: Inline -> Doc
h4 (Inline s) = Doc (tag' "h4" s)

h5 :: Inline -> Doc
h5 (Inline s) = Doc (tag' "h5" s)

h6 :: Inline -> Doc
h6 (Inline s) = Doc (tag' "h6" s)

indent :: Doc -> Doc
indent (Doc html) = Doc (tag' "blockquote" html)

list :: forall f. Foldable f => f Inline -> Doc
list =
  Doc
  <<< tag' "ul"
  <<< unDoc
  <<< foldMap (Doc <<< tag' "li" <<< unInline)

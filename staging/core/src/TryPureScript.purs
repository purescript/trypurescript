module TryPureScript
  ( Inline
  , Doc
  , DOM
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
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable, foldMap)
import Data.Monoid (class Monoid)

foreign import data DOM :: !

foreign import setInnerHTML
  :: forall eff
   . String
  -> Eff (dom :: DOM | eff) Unit

foreign import encode :: String -> String

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
link url (Inline html) = Inline (tag ("a href=\"" <> encode url <> "\" target=\"top\"") "a" html)

code :: Inline -> Inline
code (Inline s) = Inline (tag' "code" s)

newtype Doc = Doc String

unDoc :: Doc -> String
unDoc (Doc html) = html

render :: forall eff. Doc -> Eff (dom :: DOM | eff) Unit
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

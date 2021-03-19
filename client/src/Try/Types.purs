module Try.Types
  ( JS(..)
  ) where

import Data.Argonaut.Encode (class EncodeJson)
import Data.Newtype (class Newtype)

newtype JS = JS String

derive instance newtypeJS :: Newtype JS _
derive newtype instance encodeJsonJS :: EncodeJson JS

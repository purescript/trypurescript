module Try.Types
  ( JS(..)
  ) where

import Data.Newtype (class Newtype)
import Foreign.Class (class Encode)

newtype JS = JS String

derive instance newtypeJS :: Newtype JS _

derive newtype instance encodeJS :: Encode JS

module Try.Types where

import Data.Argonaut (class EncodeJson)
import Data.Newtype (class Newtype)

{-
Some common types.
Just the `JS` type for now.
-}
--
newtype JS
  = JS String

-- enable `unwrap`
derive instance newtypeJS :: Newtype JS _

derive newtype instance encodeJsonJS :: EncodeJson JS

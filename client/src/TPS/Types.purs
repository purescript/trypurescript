module TPS.Types where

import Data.Argonaut (class EncodeJson)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

{-
Some common types.
Just the `JS` type for now.
-}
--
newtype JS
  = JS String

-- enable `unwrap`
derive instance newtypeJS :: Newtype JS _

derive newtype instance showJS :: Show JS

derive newtype instance encodeJsonJS :: EncodeJson JS

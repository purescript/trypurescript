module Try.Types
  ( JS(..)
  ) where

import Data.Newtype (class Newtype)

newtype JS = JS String

derive instance newtypeJS :: Newtype JS _

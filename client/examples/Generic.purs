module Main where

import Prelude
import Effect.Console (logShow)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import TryPureScript (render, withConsole)

data Address = Address
  { city  :: String
  , state :: String
  }

data Person = Person
  { first   :: String
  , last    :: String
  , address :: Address
  }

-- Generic instances can be derived by the compiler,
-- using the derive keyword:
derive instance genericAddress :: Generic Address _

derive instance genericPerson :: Generic Person _

-- Now we can write instances for standard type classes
-- (Show, Eq, Ord) by using standard definitions
instance showAddress :: Show Address where
  show = genericShow

instance eqAddress :: Eq Address where
  eq = genericEq

instance ordAddress :: Ord Address where
  compare = genericCompare

instance showPerson :: Show Person where
  show = genericShow

instance eqPerson :: Eq Person where
  eq = genericEq

instance ordPerson :: Ord Person where
  compare = genericCompare

main = render =<< withConsole do
  logShow $ Person
    { first: "John"
    , last: "Smith"
    , address: Address
        { city: "Faketown"
        , state: "CA"
        }
    }

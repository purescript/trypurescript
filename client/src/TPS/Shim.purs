module TPS.Shim where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

{-
Allows loading additional dependencies, which are required by
some libraries.
Feel free to add additional entries to this file.
-}
--
type Shim
  = { url :: String
    , deps :: Array String
    }

shims :: Object Shim
shims =
  Object.fromFoldable
    [ Tuple "react"
        { url: "https://unpkg.com/react@16.13.1/umd/react.development.js"
        , deps: []
        }
    , Tuple "react-dom"
        { url: "https://unpkg.com/react-dom@16.13.1/umd/react-dom.development.js"
        , deps: [ "react" ]
        }
    , Tuple "react-dom/server"
        { url: "https://unpkg.com/react-dom@16.13.1/umd/react-dom-server.browser.development.js"
        , deps: [ "react" ]
        }
    , Tuple "big-integer"
        { url: "https://unpkg.com/big-integer@1.6.48/BigInteger.min.js"
        , deps: []
        }
    , Tuple "decimal.js"
        { url: "https://unpkg.com/decimal.js@10.2.0/decimal.min.js"
        , deps: []
        }
    , Tuple "uuid"
        { url: "https://cdnjs.cloudflare.com/ajax/libs/uuid/8.1.0/uuid.min.js"
        , deps: []
        }
    ]

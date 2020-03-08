module Try.Shim where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Shim =
  { url :: String
  , deps :: Array String
  }

shims :: Object Shim
shims = Object.fromFoldable
  [ Tuple "react"
      { url: "https://unpkg.com/react@16.6.3/umd/react.development.js"
      , deps: []
      }
  , Tuple "react-dom"
      { url: "https://unpkg.com/react-dom@16.6.3/umd/react-dom.development.js"
      , deps: [ "react" ]
      }
  , Tuple "react-dom/server"
      { url: "https://unpkg.com/react-dom@16.6.3/umd/react-dom-server.browser.development.js"
      , deps: [ "react" ]
      }
  ]


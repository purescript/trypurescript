module Try.Shim where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Try.Types (JS(..))

type Shim =
  { shim :: JS
  , script :: String
  , dependencies :: Array String
  }

shims :: Object Shim
shims = Object.fromFoldable
  [ Tuple "react"
      { shim: JS "module.exports = window.React;"
      , script: "//unpkg.com/react@16.6.3/umd/react.development.js"
      , dependencies: []
      }
  , Tuple "react-dom"
      { shim: JS "module.exports = window.ReactDOM;"
      , script: "//unpkg.com/react-dom@16.6.3/umd/react-dom.development.js"
      , dependencies: [ "react" ]
      }
  , Tuple "react-dom/server"
      { shim: JS "module.exports = window.ReactDOMServer;"
      , script: "//unpkg.com/react-dom@16.6.3/umd/react-dom-server.browser.development.js"
      , dependencies: [ "react" ]
      }
  ]

resolveShims :: Array String -> Array String
resolveShims = Array.nub <<< go []
  where
  go accum [] = accum
  go accum names =
    let
      shims' = names # Array.mapMaybe (flip Object.lookup shims)
      accum' = (_.script <$> shims') <> accum
      names' = shims' >>= _.dependencies
    in
      go accum' names'


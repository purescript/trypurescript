{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "try-purescript"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "functors"
  , "generics-rep"
  , "globals"
  , "identity"
  , "integers"
  , "jquery"
  , "js-timers"
  , "math"
  , "maybe"
  , "node-fs"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "random"
  , "refs"
  , "semirings"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "try-purescript"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "contravariant"
  , "control"
  , "distributive"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "free"
  , "functions"
  , "functors"
  , "generics-rep"
  , "globals"
  , "identity"
  , "integers"
  , "jquery"
  , "js-timers"
  , "lazy"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "proxy"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "refs"
  , "semirings"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "validation"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

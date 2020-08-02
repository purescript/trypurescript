{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "try-purescript"
, dependencies =
  [ "ace"
  , "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-hooks-extra"
  , "psci-support"
  , "routing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

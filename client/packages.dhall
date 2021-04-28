let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200404/packages.dhall sha256:f239f2e215d0cbd5c203307701748581938f74c4c78f4aeffa32c11c131ef7b6

let additions =
    { ace =
        { repo = "https://github.com/purescript-contrib/purescript-ace.git"
        , version = "v7.0.0"
        , dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "foreign"
          , "nullable"
          , "prelude"
          , "refs"
          , "web-html"
          , "web-uievents"
          ]
        }
    }

in  upstream // additions

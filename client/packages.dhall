let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220429/packages.dhall
        sha256:03c682bff56fc8f9d8c495ffcc6f524cbd3c89fe04778f965265c08757de8c9d

let additions =
      { debug =
        { repo = "https://github.com/working-group-purescript-es/purescript-debug.git"
        , version = "v0.15.0-update"
        , dependencies =
          [ "console", "effect", "functions", "prelude" ]
        }
      }

in  upstream // additions

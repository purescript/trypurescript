module Main where

import Prelude

import Effect (Effect)
import Data.Foldable (fold)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

main :: Effect Unit
main =
    render $ fold
      [ h1 (text "Try PureScript!")
      , p (text "Try out the examples below, or create your own!")
      , h2 (text "Examples")
      , list ([thisHomepage] <> map fromExample examples)
      , h2 (text "Share Your Code")
      , p (text "Code can be loaded from a GitHub Gist or file. To share code, simply include the Gist ID or file path in the URL as follows:")
      , indent (p (code (text "  try.ps.ai?gist=gist-id")))
      , indent (p (code (text "  try.ps.ai?github=path-to-file")))
      , p (fold
          [ text "The Gist should contain a file named "
          , code (text "Main.purs")
          , text " containing your PureScript code."
          ])
      , p (text "The github file path option is more flexible")
      ]
  where
    thisHomepage =
      link "?github=milesfrain/tps/demo/examples/Home.purs" (text "This Homepage")

    fromExample { title, gist } =
      link ("?gist=" <> gist) (text title)

    examples =
      [ { title: "Algebraic Data Types"
        , gist: "387999a4467a39744ece236e69a442ec"
        }
      , { title: "Loops"
        , gist: "429eab1e957e807f9feeddbf4f573dd0"
        }
      , { title: "Operators"
        , gist: "8395d2b421a5ca6d1056e301a6e12599"
        }
      , { title: "Records"
        , gist: "170c3ca22f0141ed06a120a12b8243af"
        }
      , { title: "Recursion"
        , gist: "659ae8a085f1cf6e52fed2c35ad93643"
        }
      , { title: "Do Notation"
        , gist: "525cb36c147d3497f652028db1214ec8"
        }
      , { title: "Type Classes"
        , gist: "b04463fd49cd4d7d385941b3b2fa226a"
        }
      , { title: "Generic Programming"
        , gist: "e3b6284959f65ac674d39aa981fcb8fb"
        }
      , { title: "QuickCheck"
        , gist: "69f7f94fe4ff3bd47f4b"
        }
      ]

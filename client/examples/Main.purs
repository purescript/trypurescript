module Main where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)

main :: Effect Unit
main =
  render $ fold
    [ h1 (text "Try PureScript!")
    , p (text "Try out the examples below, or create your own!")
    , h2 (text "Examples")
    , list (map fromExample examples)
    , h2 (text "Share Your Code")
    , p (text "The contents of the text editor are stored in the URL. To share your code you can simply copy the URL.")
    , p (text "Additionally, a PureScript file can be loaded from GitHub from a gist or a repository. To share code using a gist, include the gist ID in the URL as follows:")
    , indent (p (code (text "  try.purescript.org?gist=<gist id>")))
    , p (fold
        [ text "The gist should contain PureScript module named "
        , code (text "Main")
        , text " in a file named "
        , code (text "Main.purs")
        , text " containing your PureScript code."
        ])
    , p (text "To share code from a repository, include the path to the source file as follows:")
    , indent (p (code (text "  try.purescript.org?github=/<owner>/<repo>/<branch or tag>/<file>.purs")))
    , p (fold
        [ text "The file should be a PureScript module named "
        , code (text "Main")
        , text " containing your PureScript code."
        ])
    ]
  where
  fromExample { title, source } =
    link ("https://github.com/purescript/trypurescript/master/client/examples/" <> source) (text title)

  examples =
    [ { title: "Algebraic Data Types"
      , source: "ADTs.purs"
      }
    , { title: "Loops"
      , source: "Loops.purs"
      }
    , { title: "Operators"
      , source: "Operators.purs"
      }
    , { title: "Records"
      , source: "Records.purs"
      }
    , { title: "Recursion"
      , source: "Recursion.purs"
      }
    , { title: "Do Notation"
      , source: "DoNotation.purs"
      }
    , { title: "Type Classes"
      , source: "TypeClasses.purs"
      }
    , { title: "Generic Programming"
      , source: "Generic.purs"
      }
    , { title: "QuickCheck"
      , source: "QuickCheck.purs"
      }
    ]

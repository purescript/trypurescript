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
      , list (map fromExample examples)
      , h2 (text "Share Your Code")
      , p (text "Code can be loaded from a GitHub Gist . To share code, simply include the Gist ID in the URL as follows:")
      , indent (p (code (text "  try.purescript.org?gist=gist-id")))
      , p (fold
          [ text "The Gist should contain a file named "
          , code (text "Main.purs")
          , text " containing your PureScript code."
          ])
      ]
  where
    fromExample { title, source } =
      link ("https://github.com/purescript/trypurescript/load-from-github/client/examples/" <> source) (text title)

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

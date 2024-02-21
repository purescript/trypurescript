module Try.Config where

import Prelude

loaderUrl :: String
loaderUrl = "https://compile.purescript.org/output"

compileUrl :: String
compileUrl = "https://compile.purescript.org"

tag :: String
tag = "master"

mainGitHubExample :: String
mainGitHubExample = "purescript/trypurescript/" <> tag <> "/client/examples/Main.purs"

module Try.Config where

import Prelude

compileUrl :: String
compileUrl = "https://compile.purescript.org"

tag :: String
tag = "master"

mainGitHubExample :: String
mainGitHubExample = "/purescript/trypurescript/" <> tag <> "/client/examples/Main.purs"

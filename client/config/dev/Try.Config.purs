module Try.Config where

import Prelude

loaderUrl :: String
loaderUrl = "js/output"

compileUrl :: String
compileUrl = "http://localhost:8081"

tag :: String
tag = "load-from-github"

mainGitHubExample :: String
mainGitHubExample = "/purescript/trypurescript/" <> tag <> "/client/examples/Main.purs"

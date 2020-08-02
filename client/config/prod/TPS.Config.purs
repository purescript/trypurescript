module TPS.Config where

appDomain :: String
appDomain = "https://try.ps.ai"

tokenServerUrl :: String
tokenServerUrl = "https://tpsfunction.azurewebsites.net/api/tps?code=JmxFIJvNG9E4qFtrwyD2v40YIWAtKUt1HDxLQ9rjmP4bRafnxWjNZg=="

-- GitHub OAuth app for saving gists.
-- This is tied to a specific app domain.
clientID :: String
clientID = "3634da383bb531261af5"

compileUrl :: String
compileUrl = "https://compile.purescript.org"

loaderUrl :: String
loaderUrl = "https://compile.purescript.org/output"

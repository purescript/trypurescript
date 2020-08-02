module TPS.Config where

appDomain :: String
appDomain = "http://localhost:1234"

tokenServerUrl :: String
--tokenServerUrl = "http://localhost:7071/api/localtrigger"
tokenServerUrl = "https://localtpsfunction.azurewebsites.net/api/localtps?code=Il1fqBKydiLWqoognUIzgppwi10qfmXjkhAa75yRg5S4S10LNfsiTw=="

-- GitHub OAuth app for saving gists.
-- This is tied to a specific app domain.
clientID :: String
clientID = "6f4e10fd8cef6995ac09"

compileUrl :: String
--compileUrl = "http://localhost:8081"
compileUrl = "https://compile.purescript.org"

loaderUrl :: String
--loaderUrl = "js/output"
--loaderUrl = "http://localhost:8080"
loaderUrl = "https://compile.purescript.org/output"

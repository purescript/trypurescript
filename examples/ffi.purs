module Main where

foreign import data IO :: * -> *

foreign import log "function log(s) { return function() { console.log(s) }; }" :: String -> IO { }

main = log "Hello World!"


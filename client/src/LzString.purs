module LzString where

foreign import compressToEncodedURIComponent :: String -> String

foreign import decompressFromEncodedURIComponent :: String -> String

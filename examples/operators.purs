module Main where

import Debug.Trace

type FilePath = String

infixl 5 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) p1 p2 = p1 <> "/" <> p2

filepath :: FilePath
filepath = "usr" </> "local" </> "bin"

main = trace filepath

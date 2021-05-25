module Main where

import Prelude
import Effect.Console (log)
import TryPureScript (render, withConsole)

type FilePath = String

subdirectory :: FilePath -> FilePath -> FilePath
subdirectory p1 p2 = p1 <> "/" <> p2

-- Functions can be given an infix alias
-- The generated code will still use the original function name
infixl 5 subdirectory as </>

filepath :: FilePath
filepath = "usr" </> "local" </> "bin"

main = render =<< withConsole do
  log filepath

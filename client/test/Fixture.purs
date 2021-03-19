module Test.Fixture.Json where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser as Json
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync as FS.Sync

type Fixtures =
  { compileFailure :: Json
  , compileOtherError :: Json
  , compileSuccess :: Json
  }

readFile :: String -> Effect Json
readFile path = do
  buffer <- FS.Sync.readFile path
  file <- Buffer.toString Encoding.UTF8 buffer
  Json.jsonParser file # either (throw <<< append "Malformed fixture: ") pure

readFixtures :: Effect Fixtures
readFixtures = do
  compileFailure <- readFile "test/Fixture/compile-failure.json"
  compileOtherError <- readFile "test/Fixture/compile-other-error.json"
  compileSuccess <- readFile "test/Fixture/compile-success.json"

  pure
    { compileFailure
    , compileOtherError
    , compileSuccess
    }

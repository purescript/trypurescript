module Test.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bitraversable (ltraverse)
import Data.Either (Either, isRight)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Test.Assert (assert)
import Test.Fixture.Json (Fixtures, readFixtures)
import Try.API (CompileResult)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  fixtures <- readFixtures
  apiTests fixtures

apiTests :: Fixtures -> Effect Unit
apiTests fixtures = do
  shouldDecode (Proxy :: _ CompileResult) fixtures.compileOtherError
  shouldDecode (Proxy :: _ CompileResult) fixtures.compileFailure
  shouldDecode (Proxy :: _ CompileResult) fixtures.compileSuccess

-- | Test that a JSON response decodes successfully.
shouldDecode :: forall a. DecodeJson a => Proxy a -> Json -> Effect Unit
shouldDecode _ fixture = do
  result <- (decodeJson fixture :: Either String a) # ltraverse \errors -> do
    log "Failed to decode fixture:\n"
    logShow errors
  assert (isRight result)

module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Argonaut.Core (Json)
import Data.Bitraversable (ltraverse)
import Data.Either (Either, isRight)
import Data.Identity (Identity(..))
import Data.List.Types (NonEmptyList)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Foreign (ForeignError, unsafeToForeign)
import Foreign.Generic (class Decode, decode)
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
shouldDecode :: forall a. Decode a => Proxy a -> Json -> Effect Unit
shouldDecode _ fixture = do
  let
    result :: Either (NonEmptyList ForeignError) a
    result = un Identity $ runExceptT $ decode $ unsafeToForeign fixture

  _ <- result # ltraverse \errors -> do
    log "Failed to decode fixture:\n"
    logShow errors

  assert (isRight result)

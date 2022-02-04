module Try.App
  ( App(..)
  , Error(..)
  , Errors
  , ParApp(..)
  , runApp
  , runParApp
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as ExceptT
import Control.Parallel as Parallel
import Control.Parallel.Class (class Parallel)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either as Either
import Data.Functor.Compose (Compose(..))
import Data.Newtype as Newtype
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import Effect.Aff (Aff, ParAff)

data Error
  = FetchError String
  | FFIErrors String

type Errors = NonEmptyArray Error

newtype App a = App (ExceptT Errors Aff a)

derive newtype instance functorApp :: Functor App
derive newtype instance applyApp :: Apply App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance bindApp :: Bind App
derive newtype instance monadApp :: Monad App

runApp :: App ~> ExceptT Errors Aff
runApp (App x) = x

newtype ParApp a = ParApp (Compose ParAff (V Errors) a)

derive newtype instance functorParApp :: Functor ParApp
derive newtype instance applyParApp :: Apply ParApp
derive newtype instance applicativeParApp :: Applicative ParApp

runParApp :: ParApp ~> Compose ParAff (V Errors)
runParApp (ParApp x) = x

instance parallelParAppApp :: Parallel ParApp App where
  parallel =
    ParApp
      <<< Compose
      <<< map (Either.either V.invalid pure)
      <<< Parallel.parallel
      <<< ExceptT.runExceptT
      <<< runApp
  sequential =
    App
      <<< ExceptT
      <<< map (V.toEither)
      <<< Parallel.sequential
      <<< Newtype.unwrap
      <<< runParApp

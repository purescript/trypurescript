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
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

data Error
  = FetchError String
  | FFIErrors String

type Errors = NonEmptyArray Error

newtype App (m :: Type -> Type) a = App (ExceptT Errors m a)

derive newtype instance functorApp :: Functor m => Functor (App m)
derive newtype instance applyApp :: Monad m => Apply (App m)
derive newtype instance applicativeApp :: Monad m => Applicative (App m)
derive newtype instance bindApp :: Monad m => Bind (App m)
derive newtype instance monadApp :: Monad m => Monad (App m)
derive newtype instance monadEffectApp :: MonadEffect m => MonadEffect (App m)
derive newtype instance monadAffApp :: MonadAff m => MonadAff (App m)

runApp :: forall m. App m ~> ExceptT Errors m
runApp (App x) = x

newtype ParApp m a = ParApp (Compose m (V Errors) a)

derive newtype instance functorParApp :: Functor m => Functor (ParApp m)
derive newtype instance applyParApp :: Apply m => Apply (ParApp m)
derive newtype instance applicativeParApp :: Applicative m => Applicative (ParApp m)

runParApp :: forall f. ParApp f ~> Compose f (V Errors)
runParApp (ParApp x) = x

instance parallelParAppApp :: Parallel f m => Parallel (ParApp f) (App m) where
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

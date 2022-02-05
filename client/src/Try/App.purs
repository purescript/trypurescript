module Try.App
  ( AppT(..)
  , Error(..)
  , ParAppT
  , displayError
  , runAppT
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as ExceptT
import Control.Parallel as Parallel
import Control.Parallel.Class (class Parallel)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either as Either
import Data.Foldable (length)
import Data.Functor.Compose (Compose(..))
import Data.Newtype as Newtype
import Data.String (joinWith)
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

data Error
  = FetchError String
  | FFIErrors (NonEmptyArray String)

instance semigroupError :: Semigroup Error where
  append (FFIErrors errs) (FFIErrors errs') = FFIErrors (errs <> errs')
  append err _ = err
  
displayError :: Error -> String
displayError = case _ of
  FetchError err -> err
  FFIErrors errs -> do
    let dependencies
          | length errs == 1 = "dependency" 
          | otherwise = "dependencies"
    "FFI " <> dependencies <> " not provided: " <> joinWith ", " (NonEmpty.toArray errs)

newtype AppT (m :: Type -> Type) a = AppT (ExceptT Error m a)

derive newtype instance functorApp :: Functor m => Functor (AppT m)
derive newtype instance applyApp :: Monad m => Apply (AppT m)
derive newtype instance applicativeApp :: Monad m => Applicative (AppT m)
derive newtype instance bindApp :: Monad m => Bind (AppT m)
derive newtype instance monadApp :: Monad m => Monad (AppT m)
derive newtype instance monadEffectApp :: MonadEffect m => MonadEffect (AppT m)
derive newtype instance monadAffApp :: MonadAff m => MonadAff (AppT m)
derive newtype instance monadThrowApp :: Monad m => MonadThrow Error (AppT m)

runAppT :: forall m. AppT m ~> ExceptT Error m
runAppT (AppT x) = x

newtype ParAppT m a = ParAppT (Compose m (V Error) a)

derive newtype instance functorParApp :: Functor m => Functor (ParAppT m)
derive newtype instance applyParApp :: Apply m => Apply (ParAppT m)
derive newtype instance applicativeParApp :: Applicative m => Applicative (ParAppT m)

runParAppT :: forall f. ParAppT f ~> Compose f (V Error)
runParAppT (ParAppT x) = x

instance parallelParAppApp :: Parallel f m => Parallel (ParAppT f) (AppT m) where
  parallel =
    ParAppT
      <<< Compose
      <<< map (Either.either V.invalid pure)
      <<< Parallel.parallel
      <<< ExceptT.runExceptT
      <<< runAppT
  sequential =
    AppT
      <<< ExceptT
      <<< map (V.toEither)
      <<< Parallel.sequential
      <<< Newtype.unwrap
      <<< runParAppT

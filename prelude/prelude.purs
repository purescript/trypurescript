module Prelude where
import Prim ()
infixr 9 >>>
infixr 9 <<<
infixr 0 $
infixl 0 #
infixr 6 :
infixl 4 <$>
infixl 4 <*>
infixl 1 >>=
infixl 7 *
infixl 7 /
infixl 7 %
infixl 6 -
infixl 6 +
infix 4 ==
infix 4 /=
infixl 4 <
infixl 4 >
infixl 4 <=
infixl 4 >=
infixl 10 &
infixl 10 |
infixl 10 ^
infixr 2 ||
infixr 3 &&
infixr 5 <>
infixr 5 ++
newtype Unit = Unit {  }
data Ordering = LT  | GT  | EQ 
class Semigroup a where
  (<>) :: a -> a -> a
class BoolLike b where
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  not :: b -> b
class Bits b where
  (&) :: b -> b -> b
  (|) :: b -> b -> b
  (^) :: b -> b -> b
  shl :: b -> Prim.Number -> b
  shr :: b -> Prim.Number -> b
  zshr :: b -> Prim.Number -> b
  complement :: b -> b
class (Prelude.Eq a) <= Ord a where
  compare :: a -> a -> Prelude.Ordering
class Eq a where
  (==) :: a -> a -> Prim.Boolean
  (/=) :: a -> a -> Prim.Boolean
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  (/) :: a -> a -> a
  (%) :: a -> a -> a
  negate :: a -> a
class (Prelude.Applicative m, Prelude.Bind m) <= Monad m where
class (Prelude.Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
class (Prelude.Apply f) <= Applicative f where
  pure :: forall a. a -> f a
class (Prelude.Functor f) <= Apply f where
  (<*>) :: forall a b. f (a -> b) -> f a -> f b
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b
class Show a where
  show :: a -> Prim.String
class (Prelude.Semigroupoid a) <= Category a where
  id :: forall t. a t t
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d
foreign import unit :: Prelude.Unit
foreign import (++) :: forall s. (Prelude.Semigroup s) => s -> s -> s
foreign import (>=) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (<=) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (>) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (<) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import refIneq :: forall a. a -> a -> Prim.Boolean
foreign import refEq :: forall a. a -> a -> Prim.Boolean
foreign import ap :: forall m a b. (Prelude.Monad m) => m (a -> b) -> m a -> m b
foreign import liftM1 :: forall m a b. (Prelude.Monad m) => (a -> b) -> m a -> m b
foreign import return :: forall m a. (Prelude.Monad m) => a -> m a
foreign import liftA1 :: forall f a b. (Prelude.Applicative f) => (a -> b) -> f a -> f b
foreign import void :: forall f a. (Prelude.Functor f) => f a -> f Prelude.Unit
foreign import cons :: forall a. a -> [a] -> [a]
foreign import (:) :: forall a. a -> [a] -> [a]
foreign import (#) :: forall a b. a -> (a -> b) -> b
foreign import ($) :: forall a b. (a -> b) -> a -> b
foreign import (>>>) :: forall a b c d. (Prelude.Semigroupoid a) => a b c -> a c d -> a b d
foreign import asTypeOf :: forall a. a -> a -> a
foreign import const :: forall a b. a -> b -> a
foreign import flip :: forall a b c. (a -> b -> c) -> b -> a -> c
foreign import instance semigroupoidArr :: Prelude.Semigroupoid Prim.Function
foreign import instance categoryArr :: Prelude.Category Prim.Function
foreign import instance showUnit :: Prelude.Show Prelude.Unit
foreign import instance showString :: Prelude.Show Prim.String
foreign import instance showBoolean :: Prelude.Show Prim.Boolean
foreign import instance showNumber :: Prelude.Show Prim.Number
foreign import instance showArray :: (Prelude.Show a) => Prelude.Show [a]
foreign import instance functorArr :: Prelude.Functor (Prim.Function r)
foreign import instance applyArr :: Prelude.Apply (Prim.Function r)
foreign import instance applicativeArr :: Prelude.Applicative (Prim.Function r)
foreign import instance bindArr :: Prelude.Bind (Prim.Function r)
foreign import instance monadArr :: Prelude.Monad (Prim.Function r)
foreign import instance numNumber :: Prelude.Num Prim.Number
foreign import instance eqUnit :: Prelude.Eq Prelude.Unit
foreign import instance eqString :: Prelude.Eq Prim.String
foreign import instance eqNumber :: Prelude.Eq Prim.Number
foreign import instance eqBoolean :: Prelude.Eq Prim.Boolean
foreign import instance eqArray :: (Prelude.Eq a) => Prelude.Eq [a]
foreign import instance eqOrdering :: Prelude.Eq Prelude.Ordering
foreign import instance showOrdering :: Prelude.Show Prelude.Ordering
foreign import instance ordUnit :: Prelude.Ord Prelude.Unit
foreign import instance ordBoolean :: Prelude.Ord Prim.Boolean
foreign import instance ordNumber :: Prelude.Ord Prim.Number
foreign import instance ordString :: Prelude.Ord Prim.String
foreign import instance ordArray :: (Prelude.Ord a) => Prelude.Ord [a]
foreign import instance bitsNumber :: Prelude.Bits Prim.Number
foreign import instance boolLikeBoolean :: Prelude.BoolLike Prim.Boolean
foreign import instance semigroupUnit :: Prelude.Semigroup Prelude.Unit
foreign import instance semigroupString :: Prelude.Semigroup Prim.String
foreign import instance semigroupArr :: (Prelude.Semigroup s') => Prelude.Semigroup (s -> s')
module Prelude.Unsafe where
import Prim ()
import Prelude ()
foreign import unsafeIndex :: forall a. [a] -> Prim.Number -> a
module Data.Function where
import Prim ()
import Prelude ()
foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *
foreign import data Fn4 :: * -> * -> * -> * -> * -> *
foreign import data Fn3 :: * -> * -> * -> * -> *
foreign import data Fn2 :: * -> * -> * -> *
foreign import data Fn1 :: * -> * -> *
foreign import data Fn0 :: * -> *
foreign import runFn10 :: forall a b c d e f g h i j k. Data.Function.Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
foreign import runFn9 :: forall a b c d e f g h i j. Data.Function.Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
foreign import runFn8 :: forall a b c d e f g h i. Data.Function.Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i
foreign import runFn7 :: forall a b c d e f g h. Data.Function.Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h
foreign import runFn6 :: forall a b c d e f g. Data.Function.Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g
foreign import runFn5 :: forall a b c d e f. Data.Function.Fn5 a b c d e f -> a -> b -> c -> d -> e -> f
foreign import runFn4 :: forall a b c d e. Data.Function.Fn4 a b c d e -> a -> b -> c -> d -> e
foreign import runFn3 :: forall a b c d. Data.Function.Fn3 a b c d -> a -> b -> c -> d
foreign import runFn2 :: forall a b c. Data.Function.Fn2 a b c -> a -> b -> c
foreign import runFn1 :: forall a b. Data.Function.Fn1 a b -> a -> b
foreign import runFn0 :: forall a. Data.Function.Fn0 a -> a
foreign import mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Data.Function.Fn10 a b c d e f g h i j k
foreign import mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Data.Function.Fn9 a b c d e f g h i j
foreign import mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Data.Function.Fn8 a b c d e f g h i
foreign import mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Data.Function.Fn7 a b c d e f g h
foreign import mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Data.Function.Fn6 a b c d e f g
foreign import mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Data.Function.Fn5 a b c d e f
foreign import mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Data.Function.Fn4 a b c d e
foreign import mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Data.Function.Fn3 a b c d
foreign import mkFn2 :: forall a b c. (a -> b -> c) -> Data.Function.Fn2 a b c
foreign import mkFn1 :: forall a b. (a -> b) -> Data.Function.Fn1 a b
foreign import mkFn0 :: forall a. (Prelude.Unit -> a) -> Data.Function.Fn0 a
foreign import on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
module Data.Eq where
import Prim ()
import Prelude ()
newtype Ref (a :: *) = Ref a
foreign import liftRef :: forall a b. (a -> a -> b) -> Data.Eq.Ref a -> Data.Eq.Ref a -> b
foreign import instance eqRef :: Prelude.Eq (Data.Eq.Ref a)
foreign import instance functorRef :: Prelude.Functor Data.Eq.Ref
module Control.Monad.Eff where
import Prim ()
import Prelude ()
type Pure (a :: *) = forall e. Control.Monad.Eff.Eff e a
foreign import data Eff :: # ! -> * -> *
foreign import foreachE :: forall e a. [a] -> (a -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import forE :: forall e. Prim.Number -> Prim.Number -> (Prim.Number -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import whileE :: forall e a. Control.Monad.Eff.Eff e Prim.Boolean -> Control.Monad.Eff.Eff e a -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import untilE :: forall e. Control.Monad.Eff.Eff e Prim.Boolean -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import runPure :: forall a. Control.Monad.Eff.Pure a -> a
foreign import bindE :: forall e a b. Control.Monad.Eff.Eff e a -> (a -> Control.Monad.Eff.Eff e b) -> Control.Monad.Eff.Eff e b
foreign import returnE :: forall e a. a -> Control.Monad.Eff.Eff e a
foreign import instance functorEff :: Prelude.Functor (Control.Monad.Eff.Eff e)
foreign import instance applyEff :: Prelude.Apply (Control.Monad.Eff.Eff e)
foreign import instance applicativeEff :: Prelude.Applicative (Control.Monad.Eff.Eff e)
foreign import instance bindEff :: Prelude.Bind (Control.Monad.Eff.Eff e)
foreign import instance monadEff :: Prelude.Monad (Control.Monad.Eff.Eff e)
module Control.Monad.Eff.Unsafe where
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
foreign import unsafeInterleaveEff :: forall eff1 eff2 a. Control.Monad.Eff.Eff eff1 a -> Control.Monad.Eff.Eff eff2 a
module Control.Monad.ST where
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
foreign import data STArray :: * -> * -> *
foreign import data STRef :: * -> * -> *
foreign import data ST :: * -> !
foreign import runSTArray :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Control.Monad.ST.STArray h a)) -> Control.Monad.Eff.Eff r [a]
foreign import runST :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a) -> Control.Monad.Eff.Eff r a
foreign import pokeSTArray :: forall a h r. Control.Monad.ST.STArray h a -> Prim.Number -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
foreign import peekSTArray :: forall a h r. Control.Monad.ST.STArray h a -> Prim.Number -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
foreign import newSTArray :: forall a h r. Prim.Number -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Control.Monad.ST.STArray h a)
foreign import writeSTRef :: forall a h r. Control.Monad.ST.STRef h a -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
foreign import modifySTRef :: forall a h r. Control.Monad.ST.STRef h a -> (a -> a) -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
foreign import readSTRef :: forall a h r. Control.Monad.ST.STRef h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
foreign import newSTRef :: forall a h r. a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Control.Monad.ST.STRef h a)
module Debug.Trace where
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
foreign import data Trace :: !
foreign import print :: forall a r. (Prelude.Show a) => a -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit
foreign import trace :: forall r. Prim.String -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit
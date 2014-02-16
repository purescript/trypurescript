module Prelude where

  infixr 0 $

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  class Monad m where
    ret :: forall a. a -> m a
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  infixl 7 *
  infixl 7 /
  infixl 7 %

  infixl 6 -
  infixl 6 +

  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    negate :: a -> a

  foreign import numAdd "function numAdd(n1) {\
                        \  return function(n2) {\
                        \    return n1 + n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numSub "function numSub(n1) {\
                        \  return function(n2) {\
                        \    return n1 - n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numMul "function numMul(n1) {\
                        \  return function(n2) {\
                        \    return n1 * n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numDiv "function numDiv(n1) {\
                        \  return function(n2) {\
                        \    return n1 / n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numMod "function numMod(n1) {\
                        \  return function(n2) {\
                        \    return n1 % n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numNegate "function numNegate(n) {\
                           \  return -n;\
                           \}" :: Number -> Number

  instance Num Number where
    (+) = numAdd
    (-) = numSub
    (*) = numMul
    (/) = numDiv
    (%) = numMod
    negate = numNegate

  infixl 4 ==
  infixl 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  foreign import unsafeRefEq "function unsafeRefEq(r1) {\
                             \  return function(r2) {\
                             \    return r1 === r2;\
                             \  };\
                             \}" :: forall a. a -> a -> Boolean

  foreign import unsafeRefIneq "function unsafeRefIneq(r1) {\
                               \  return function(r2) {\
                               \    return r1 !== r2;\
                               \  };\
                               \}" :: forall a. a -> a -> Boolean

  instance Eq String where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance Eq Number where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance Eq Boolean where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance (Eq a) => Eq [a] where
    (==) [] [] = true
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = false
    (/=) xs ys = not (xs == ys)

  infixl 4 <
  infixl 4 >
  infixl 4 <=
  infixl 4 >=

  class Ord a where
    (<) :: a -> a -> Boolean
    (>) :: a -> a -> Boolean
    (<=) :: a -> a -> Boolean
    (>=) :: a -> a -> Boolean

  foreign import numLess "function numLess(n1) {\
                         \  return function(n2) {\
                         \    return n1 < n2;\
                         \  };\
                         \}" :: Number -> Number -> Boolean

  foreign import numLessEq "function numLessEq(n1) {\
                           \  return function(n2) {\
                           \    return n1 <= n2;\
                           \  };\
                           \}" :: Number -> Number -> Boolean

  foreign import numGreater "function numGreater(n1) {\
                            \  return function(n2) {\
                            \    return n1 > n2;\
                            \  };\
                            \}" :: Number -> Number -> Boolean

  foreign import numGreaterEq "function numGreaterEq(n1) {\
                              \  return function(n2) {\
                              \    return n1 >= n2;\
                              \  };\
                              \}" :: Number -> Number -> Boolean

  instance Ord Number where
    (<) = numLess
    (>) = numGreater
    (<=) = numLessEq
    (>=) = numGreaterEq

  infixl 10 &
  infixl 10 |
  infixl 10 ^

  class Bits b where
    (&) :: b -> b -> b
    (|) :: b -> b -> b
    (^) :: b -> b -> b
    shl :: b -> Number -> b
    shr :: b -> Number -> b
    zshr :: b -> Number -> b
    complement :: b -> b

  foreign import numShl "function numShl(n1) {\
                        \  return function(n2) {\
                        \    return n1 << n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numShr "function numShr(n1) {\
                        \  return function(n2) {\
                        \    return n1 >> n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numZshr "function numZshr(n1) {\
                          \  return function(n2) {\
                          \    return n1 >>> n2;\
                          \  };\
                          \}" :: Number -> Number -> Number

  foreign import numAnd "function numAnd(n1) {\
                        \  return function(n2) {\
                        \    return n1 & n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numOr "function numOr(n1) {\
                       \  return function(n2) {\
                       \    return n1 | n2;\
                       \  };\
                       \}" :: Number -> Number -> Number

  foreign import numXor "function numXor(n1) {\
                        \  return function(n2) {\
                        \    return n1 ^ n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

  foreign import numComplement "function numComplement(n) {\
                               \  return ~n;\
                               \}" :: Number -> Number

  instance Bits Number where
    (&) = numAnd
    (|) = numOr
    (^) = numXor
    shl = numShl
    shr = numShr
    zshr = numZshr
    complement = numComplement

  infixl 8 !!

  foreign import (!!) "function $bang$bang(xs) {\
                      \  return function(n) {\
                      \    return xs[n];\
                      \  };\
                      \}" :: forall a. [a] -> Number -> a

  infixr 2 ||
  infixr 3 &&

  class BoolLike b where
    (&&) :: b -> b -> b
    (||) :: b -> b -> b
    not :: b -> b

  foreign import boolAnd "function boolAnd(b1) {\
                         \  return function(b2) {\
                         \    return b1 && b2;\
                         \  };\
                         \}"  :: Boolean -> Boolean -> Boolean

  foreign import boolOr "function boolOr(b1) {\
                        \  return function(b2) {\
                        \    return b1 || b2;\
                        \  };\
                        \}" :: Boolean -> Boolean -> Boolean

  foreign import boolNot "function boolNot(b) {\
                         \  return !b;\
                         \}" :: Boolean -> Boolean

  instance BoolLike Boolean where
    (&&) = boolAnd
    (||) = boolOr
    not = boolNot

  infixr 5 ++

  foreign import (++) "function $plus$plus(s1) {\
                      \  return function(s2) {\
                      \    return s1 + s2;\
                      \  };\
                      \}" :: String -> String -> String

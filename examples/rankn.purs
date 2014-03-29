module Main where

type Nat = forall a. a -> (a -> a) -> a

zero :: Nat
zero a _ = a

succ :: Nat -> Nat
succ n a f = f (n a f)

type Lens a b = forall f. (a -> f a) -> b -> f b

compose :: forall a b c. Lens a b -> Lens b c -> Lens a c
compose l1 l2 f = l2 (l1 f)


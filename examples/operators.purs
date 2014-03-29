module Main where

import Prelude ()

infixl 5 >>>

(>>>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(>>>) f g a = g (f a)

foreign import foo :: String -> Number
foreign import bar :: Number -> Boolean

test = foo >>> bar

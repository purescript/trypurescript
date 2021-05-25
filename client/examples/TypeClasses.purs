module Main where

import Prelude
import Effect.Console (log)
import TryPureScript (render, withConsole)

-- A type class for types which can be used with
-- string interpolation.
class Interpolate a where
  interpolate :: a -> String

instance interpolateString :: Interpolate String where
  interpolate = identity

instance interpolateInt :: Interpolate Int where
  interpolate = show

-- A type class for printf functions
-- (each list of argument types will define a type class instance)
class Printf r where
  printfWith :: String -> r

-- An instance for no function arguments
-- (just return the accumulated string)
instance printfString :: Printf String where
  printfWith = identity

-- An instance for adding another argument whose
-- type is an instance of Interpolate
instance printfShow :: (Interpolate a, Printf r) => Printf (a -> r) where
  printfWith s a = printfWith (s <> interpolate a)

-- Our generic printf function
printf :: forall r. (Printf r) => r
printf = printfWith ""

-- Now we can create custom formatters using different argument
-- types
debug :: String -> Int -> String -> String
debug uri status msg = printf "[" uri "] " status ": " msg

main = render =<< withConsole do
  log $ debug "http://www.purescript.org" 200 "OK"
  log $ debug "http://bad.purescript.org" 404 "Not found"

module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Printf r where
  printfWith :: String -> r
  
instance printfString :: Printf String where
  printfWith = id
  
instance printfShow :: (Show a, Printf r) => Printf (a -> r) where
  printfWith s a = printfWith (s <> show a)

-- | A newtype for literal strings
newtype Lit = Lit String

instance showLit :: Show Lit where
  show (Lit s) = s

printf :: forall r. (Printf r) => r
printf = printfWith ""

-- | We can create custom formatters using different argument
-- | types.
debug :: String -> Int -> String -> String
debug uri status msg  = 
  printf (Lit "[") 
         uri
         (Lit "] ") 
         status 
         (Lit ": ") 
         msg

main = do
  log $ debug "http://www.purescript.org" 200 "OK"
  log $ debug "http://bad.purescript.org" 404 "Not found"
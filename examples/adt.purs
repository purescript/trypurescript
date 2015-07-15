module Main where
    
import Prelude
import Control.Monad.Eff.Console (log)

data Person = Person String Int 
                        
showPerson :: Person -> String
showPerson (Person name age) =
  name ++ ", aged " ++ show age
        
person :: Person
person = Person "John Smith" 30

main = log (showPerson person)

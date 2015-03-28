module Main where
  
import Debug.Trace

data Person = Person String Number 
            
showPerson :: Person -> String
showPerson (Person name age) =
  name ++ ", aged " ++ show age
    
person :: Person
person = Person "John Smith" 30

main = trace (showPerson person)

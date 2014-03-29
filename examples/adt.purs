module Main where

data Person = Person String Number 

showPerson :: Person -> String
showPerson (Person name age) =
  name ++ ", aged " ++ show age

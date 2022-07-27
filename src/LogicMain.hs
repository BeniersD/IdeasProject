module Main where

import Ideas.Common.Library
import Ideas.Main.Default
import LogicExercices

dr :: DomainReasoner
dr = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
   { exercises = [Some minimalExercise, Some basicExercise, Some evalExercise]
   , services  = myServices
   }

myServices :: [Service]
myServices = metaServiceList dr ++ serviceList

main :: IO ()
main = defaultMain dr
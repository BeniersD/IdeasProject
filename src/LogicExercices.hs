module LogicExercices where

import Data.Maybe
import Data.List           (group, sort, sortBy, (\\))
import Domain.Logic.Formula
import Ideas.Common.Library
import Ideas.Main.Default
import LogicReductionStrategies
import LogicTestCases
import LogicReductionRules
import LogicFunctions

main :: IO ()
main = defaultMain dr

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Derivation functions
-------------------------------------------------------------------------------------------------------------------------------------------------
getMatchingStrategy :: SLogic -> SLogic -> LabeledStrategy (Context SLogic)
getMatchingStrategy x y =  derivToStrategy $ derivDiff x y 

derivStepsList :: SLogic -> [Rule (Context SLogic)]
derivStepsList x = 
      case (defaultDerivation basicExercise x) of
        Just d  -> [ x | (x,y) <- steps d]   
        Nothing -> []

derivTermsList :: SLogic -> [Context SLogic]
derivTermsList x = 
      case (defaultDerivation basicExercise x) of
        Just d  -> terms d 
        Nothing -> []

derivDiff :: SLogic -> SLogic -> [Rule (Context SLogic)]
derivDiff x y = xs 
      where
          s1 = reverse $ derivStepsList x
          t1 = reverse $ derivTermsList x
          s2 = reverse $ derivStepsList y
          t2 = reverse $ derivTermsList y
          xs = case head t1 == head t2 of
                  True  -> removeDuplicates $ removeMatching s1 s2
                  False -> []

derivToStrategy :: [Rule (Context SLogic)] -> LabeledStrategy (Context SLogic)
derivToStrategy [] = label "failure" $ failS
derivToStrategy xs | length xs == 1 = label d $ s  
      where
          r = head xs
          d = showId r
          s = repeatS ( somewhere r)
derivToStrategy xs | otherwise       = label d $ s
      where
          ys = case (elem (liftToContext ruleDeMorganOr) xs || elem (liftToContext ruleDeMorganAnd) xs) of
                  True  -> [ruleDeMorgan] ++ filter (\x -> showId x /= "single.demorgan.or" && showId x /= "single.demorgan.and" ) xs
                  False -> xs
          ms = multiStrategy Choice ys
          s = repeatS ( somewhere ms)
          d = showId ms
          
derivToStringList :: [Rule (Context SLogic)] -> [String]
derivToStringList xs = map show xs

removeMatching :: [Rule (Context SLogic)] -> [Rule (Context SLogic)] -> [Rule (Context SLogic)]
removeMatching (x:xs) (y:ys) | x == y    = removeMatching xs ys
removeMatching xs ys                     = xs ++ ys 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercices
-------------------------------------------------------------------------------------------------------------------------------------------------
minimalExercise, basicExercise, evalExercise :: Exercise SLogic
minimalExercise = emptyExercise
                { 
                  exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
                  strategy      = stratToDnf, 
                  prettyPrinter = show
                }
basicExercise   = emptyExercise
                { 
                  exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
                  strategy      = stratToDnf, 
                  navigation    = termNavigator, 
                  prettyPrinter = show
                }
evalExercise    = emptyExercise
                { 
                  exerciseId    = describe "Evaluate an expression (full)" $ newId "eval.full",
                  status        = Experimental,
                  strategy      = stratToDnf,
                  prettyPrinter = show,
                  navigation    = termNavigator,
                  --, parser        = readM
                  equivalence   = withoutContext eqExpr,
                  similarity    = withoutContext (==),
                  ready         = predicate (const True),
                  examples      = examplesFor Easy (negationsTestSet ++ layerTestSet ++ commutativityTestSet) 
                }

dr :: DomainReasoner
dr = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
                { 
                  exercises = [
                                 Some minimalExercise,
                                 Some basicExercise,
                                 Some evalExercise
                              ],
                  services      = myServices
                }

myServices :: [Service]
myServices = metaServiceList dr ++ serviceList



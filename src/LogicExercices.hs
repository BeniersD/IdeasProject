module LogicExercices where

import Data.Maybe
import Domain.Logic.Formula
import Ideas.Common.Library
import Ideas.Main.Default
import LogicReductionStrategies
import LogicTestCases

main :: IO ()
main = defaultMain dr


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

execStrategy :: LabeledStrategy (Context SLogic) -> SLogic -> String
execStrategy s x = show $ applyD s (newContext $ termNavigator x)

eqExpr :: SLogic -> SLogic -> Bool
eqExpr x y = (execStrategy stratAC x) == (execStrategy stratAC y)

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



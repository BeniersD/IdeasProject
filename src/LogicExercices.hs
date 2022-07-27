module LogicExercices (minimalExercise, basicExercise, evalExercise
   ) where

import Data.Function
import Data.Typeable
import Ideas.Common.Library
import Domain.Logic.Formula  hiding (not)
import Ideas.Main.Default
import Ideas.Utils.Prelude (splitsWithElem, readM)
import LogicReductionRules
import LogicReductionStrategies
import LogicFunctions
import LogicTestCases

-------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = defaultMain (dr (evalStrategy ruleAC Single))

dr :: LabeledStrategy (Context SLogic) -> DomainReasoner
dr x = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
   { exercises = [Some (minimalExercise x), Some (basicExercise x), Some (evalExercise x)]
   , services  = myServices x
   }

myServices :: LabeledStrategy (Context SLogic) -> [Service]
myServices x = metaServiceList (dr x) ++ serviceList

minimalExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
minimalExercise x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
                        strategy      = x, 
                        prettyPrinter = show
                     }

basicExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
basicExercise   x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
                        strategy      = x, 
                        navigation    = termNavigator, 
                        prettyPrinter = show
                     }

evalExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
evalExercise    x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (full)" $ newId "eval.full", 
                        status        = Experimental, 
                        strategy      = x, 
                        prettyPrinter = show, 
                        navigation    = termNavigator, 
                        --parser        = readM, 
                        equivalence   = withoutContext eqExpr,
                        -- properties    = setProperty "indistinguishability" withoutContext (indExpr x),
                        -- ->? -
                        similarity    = withoutContext (==), 
                        ready         = predicate (const True),-- isTerm, 
                        examples      = examplesFor Easy commutativityTestSet
                     }

eqExpr :: SLogic -> SLogic -> Bool
eqExpr x y = x == y

{--
indistinguishabilityS :: Service
indistinguishabilityS = makeService "basic.indistinguishability"
   "Tests whether two terms which are distinguishable entities in a finer-grained granule are indistinguishable in a coarser-grained granule." $
   indistinguishability ::: tExercise .-> tContext .-> tContext .-> tBool
--}

--indExpr :: Eq a => LSExpr a -> SLogic -> SLogic -> Bool
--indExpr = \f -> (\p -> (\q -> f p == f q))
--indExpr f = (==) `on` f

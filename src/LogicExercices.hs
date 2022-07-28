{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicExercices where

import Data.Function
import Data.Typeable
import Ideas.Common.Library
import Domain.Logic.Formula  hiding (not)
import Ideas.Main.Default
import Ideas.Utils.Prelude (splitsWithElem, readM)
import LogicReductionStrategies
import LogicTestCases

data EvaluationType = Single | SomeWhere | SomeWhereRepeatS | SomeWhereRepeat1 | RepeatS | Repeat1 

class LogicRuleConversion a where
    type Out a     :: * 
    ruleToStrategy :: a -> Out a

instance LogicRuleConversion (Rule SLogic) where  
    type Out (Rule SLogic) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = ruleToStrategy (liftToContext x)

instance LogicRuleConversion (Rule (Context SLogic)) where 
    type Out (Rule (Context SLogic)) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = label (showId x) $ x

class LogicEvaluationStrategy a where
    type Out2 a  :: * 
    evalStrategy :: a -> Out2 a
    
instance LogicEvaluationStrategy (Rule SLogic) where  
    type Out2 (Rule SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (ruleToStrategy r) 

instance LogicEvaluationStrategy (Rule (Context SLogic)) where  
    type Out2 (Rule (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (ruleToStrategy r)
    
instance LogicEvaluationStrategy (LabeledStrategy SLogic) where  
    type Out2 (LabeledStrategy SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (liftToContext r)

instance LogicEvaluationStrategy (LabeledStrategy (Context SLogic)) where  
    type Out2 (LabeledStrategy (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r Single           = evalStrategyG ("Evaluate - " ++ showId r)                     r 
    evalStrategy r SomeWhere        = evalStrategyG ("Evaluate somewhere - " ++ showId r)           (somewhere r)
    evalStrategy r RepeatS          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeatS   r) 
    evalStrategy r Repeat1          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeat1   r) 
    evalStrategy r SomeWhereRepeatS = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeatS   (somewhere r))
    evalStrategy r SomeWhereRepeat1 = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeat1   (somewhere r))

evalStrategyG :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
evalStrategyG l s       = label l $ s

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

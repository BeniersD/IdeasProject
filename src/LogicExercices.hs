module LogicExercices where

import Data.Function
import Ideas.Common.Library
import Ideas.Main.Default
import Domain.Logic.Formula  hiding (not)
import Ideas.Utils.Prelude (splitsWithElem, readM)
import LogicReductionRules
import LogicReductionStrategies

minimalExercise :: Show a => LabeledStrategy (Logic a) -> Exercise (Logic a)
minimalExercise x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
      strategy      = liftToContext x, 
      prettyPrinter = show
   }

basicExercise :: (IsTerm a, Show a) => LabeledStrategy (Logic a) -> Exercise (Logic a)
basicExercise x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic", 
      strategy      = evalStrategy x, 
      navigation    = termNavigator, 
      prettyPrinter = show
   }

eqExpr :: (Eq a) => Logic a -> Logic a -> Bool
eqExpr x y = x == y

isLogicTerm :: Logic a -> Bool
isLogicTerm ( Var _ )     = True
isLogicTerm ( _ :->:  _ ) = True
isLogicTerm ( _ :<->: _ ) = True
isLogicTerm ( _ :&&:  _ ) = True
isLogicTerm ( _ :||:  _ ) = True
isLogicTerm ( Not _ ) = True
isLogicTerm T             = True
isLogicTerm F             = True

{--
indistinguishabilityS :: Service
indistinguishabilityS = makeService "basic.indistinguishability"
   "Tests whether two terms which are distinguishable entities in a finer-grained granule are indistinguishable in a coarser-grained granule." $
   indistinguishability ::: tExercise .-> tContext .-> tContext .-> tBool
--}

--indExpr :: Eq a => LSExpr a -> Logic a -> Logic a -> Bool
--indExpr = \f -> (\p -> (\q -> f p == f q))
--indExpr f = (==) `on` f


evalExercise :: (Show a, IsTerm a, Eq a) => LabeledStrategy (Logic a) -> Exercise (Logic a)
evalExercise x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (full)" $ newId "eval.full", 
      status        = Experimental, 
      strategy      = evalStrategy x, 
      prettyPrinter = show, 
      navigation    = termNavigator, 
      --parser        = readM, 
      equivalence   = withoutContext eqExpr,
      -- properties    = setProperty "indistinguishability" withoutContext (indExpr x),
      -- ->? 
      similarity    = withoutContext (==), 
      ready         = predicate isLogicTerm, 
      examples      = examplesFor Easy []
   }
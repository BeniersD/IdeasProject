{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LogicExercices (minimalExercise, minimalExercise', basicExercise, basicExercise'--, evalExercise
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


class LogicExercise a where
    type Out a :: * 
    evalStrategy :: a -> Out a

instance LogicExercise (LabeledStrategy SLogic) where  
    type Out (LabeledStrategy SLogic) = (LabeledStrategy (Context SLogic))
    evalStrategy = \x -> liftToContext x

instance LogicExercise (LabeledStrategy (Context SLogic)) where 
    type Out (LabeledStrategy (Context SLogic)) = (LabeledStrategy (Context SLogic)) 
    evalStrategy = \x -> x


minimalExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
minimalExercise x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
      strategy      = evalStrategy x, 
      prettyPrinter = show
   }

minimalExercise' :: LabeledStrategy SLogic -> Exercise SLogic
minimalExercise' x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
      strategy      = evalStrategy x, 
      prettyPrinter = show
   }

basicExercise ::  LabeledStrategy (Context SLogic) -> Exercise (SLogic)
basicExercise x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
      strategy      = evalStrategy x, 
      navigation    = termNavigator, 
      prettyPrinter = show
   }

basicExercise' ::  LabeledStrategy SLogic -> Exercise (SLogic)
basicExercise' x = emptyExercise
   { 
      exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
      strategy      = evalStrategy x, 
      navigation    = termNavigator, 
      prettyPrinter = show
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


--evalExercise :: LabeledStrategy a -> Exercise (SLogic)
--evalExercise x = emptyExercise
--   { 
--      exerciseId    = describe "Evaluate an expression (full)" $ newId "eval.full", 
--      status        = Experimental, 
--      strategy      = evalStrategy x, 
--      prettyPrinter = show, 
--      navigation    = termNavigator, 
--      --parser        = readM, 
--      equivalence   = withoutContext eqExpr,
--      -- properties    = setProperty "indistinguishability" withoutContext (indExpr x),
      -- ->? -
--      similarity    = withoutContext (==), 
--      ready         = predicate isWff, 
--      examples      = examplesFor Easy []
--   }
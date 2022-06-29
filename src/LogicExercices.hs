{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LogicExercices (minimalExercise, basicExercise --, evalExercise
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
   type Out3 a      :: *
   minimalExercise :: a -> Out3 a
   basicExercise   :: a -> Out3 a
   evalExercise    :: a -> Out3 a

instance LogicExercise (Rule SLogic) where
   type Out3 (Rule SLogic) = Exercise SLogic
   minimalExercise x = minimalExercise (ruleToStrategy x)
   basicExercise   x = basicExercise   (ruleToStrategy x)
   evalExercise    x = evalExercise    (ruleToStrategy x)

instance LogicExercise (Rule (Context SLogic)) where
   type Out3 (Rule (Context SLogic)) = Exercise SLogic
   minimalExercise x = minimalExercise (ruleToStrategy x)
   basicExercise   x = basicExercise   (ruleToStrategy x)
   evalExercise    x = evalExercise    (ruleToStrategy x)

instance LogicExercise (LabeledStrategy SLogic) where
   type Out3 (LabeledStrategy SLogic) = Exercise SLogic
   minimalExercise x = minimalExercise (liftToContext x)
   basicExercise   x = basicExercise   (liftToContext x)
   evalExercise    x = evalExercise    (liftToContext x)

instance LogicExercise (LabeledStrategy (Context SLogic)) where
   type Out3 (LabeledStrategy (Context SLogic)) = Exercise SLogic
   minimalExercise x = emptyExercise
                        { 
                           exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
                           strategy      = x, 
                           prettyPrinter = show
                        }
   basicExercise   x = emptyExercise
                        { 
                           exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
                           strategy      = x, 
                           navigation    = termNavigator, 
                           prettyPrinter = show
                        }
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
                           ready         = predicate isWff, 
                           examples      = examplesFor Easy []
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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicReductionStrategies where

import Ideas.Common.Library hiding (layer)
import Data.List
import Data.Maybe
import Domain.Logic.Formula 
import LogicReductionRules
import Ideas.Common.Traversal.Navigator
import qualified Ideas.Common.Strategy.Combinators as Combinators
import LogicFunctions

--------------------------------------------------------------------------------------------------------------------------------------
-- Strategies: under construction
--------------------------------------------------------------------------------------------------------------------------------------
data LayerOrd = TopFirst | LayerFirst
data EvaluationType = Single | SomeWhere | SomeWhereRepeatS | SomeWhereRepeat1 | RepeatS | Repeat1 

class LogicRuleConversion a where
    type Out a     :: * 
    ruleToStrategy :: a -> Out a

instance LogicRuleConversion (Rule SLogic) where  
    type Out (Rule SLogic) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x = ruleToStrategy (liftToContext x)

instance LogicRuleConversion (Rule (Context SLogic)) where 
    type Out (Rule (Context SLogic)) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x = label (showId x) $ x

class LogicEvaluationStrategy a where
    type Out2 a  :: * 
    evalStrategy :: a -> Out2 a
    
instance LogicEvaluationStrategy (Rule SLogic) where  
    type Out2 (Rule SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r = evalStrategy (ruleToStrategy r) 

instance LogicEvaluationStrategy (Rule (Context SLogic)) where  
    type Out2 (Rule (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r = evalStrategy (ruleToStrategy r)
    
instance LogicEvaluationStrategy (LabeledStrategy SLogic) where  
    type Out2 (LabeledStrategy SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r = evalStrategy (liftToContext r)

instance LogicEvaluationStrategy (LabeledStrategy (Context SLogic)) where  
    type Out2 (LabeledStrategy (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r Single           = evalStrategyG ("Evaluate - " ++ showId r)                     r 
    evalStrategy r SomeWhere        = evalStrategyG ("Evaluate somewhere - " ++ showId r)           (somewhere r)
    evalStrategy r RepeatS          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeatS   r) 
    evalStrategy r Repeat1          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeat1   r) 
    evalStrategy r SomeWhereRepeatS = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeatS   (somewhere r))
    evalStrategy r SomeWhereRepeat1 = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeat1   (somewhere r))

evalStrategyG :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
evalStrategyG l s = label l $ s

evalCondOnTerm :: (SLogic -> Bool) -> Strategy (Context SLogic)
evalCondOnTerm c = check (maybe False c . currentInContext)


stratCommutativitySpec  = label "Rewrite Strategy Commutativity-Ordered"              $ (check (not . isAssoCommOrdered) .*. ruleCommutativity)               |> failS

ruleassociativityReverse :: Rule SLogic
ruleassociativityReverse = createRule "Associativity Reversed"           "single.associativity.reversed"          f
    where
        f ::  SLogic -> Maybe (SLogic)
        f (p :||: (q:||: r)) = Just ((p :||: q) :||: r) 
        f (p :&&: (q :&&: r)) = Just ((p :&&: q) :&&: r)
        f _ = Nothing

ruleAssociativityCommutativity :: Rule (Context SLogic)
ruleAssociativityCommutativity = convertToRule "Associativity Commutativity Ordered"           "single.associativity.commutative.ordered"           (evalStrategy stratAssociativityCommutativity SomeWhereRepeat1)

stratAssociativityCommutativity :: LabeledStrategy (Context SLogic)
stratAssociativityCommutativity = label "Rewrite Strategy Associativity-Commutativity-Ordered" $  s
    where
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :||: q)                             = True
        f (p :||: (q :||: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :||: skipNegations q) = True
        f (p :&&: (q :&&: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :&&: q)                             = True
        f (p :&&: (q :&&: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :&&: skipNegations q) = True
        f _                                                                                                                      = False
        lar = liftToContext ruleassociativityReverse
        la  = liftToContext ruleAssociativity
        s = liftToContext ruleAssociativity |> liftToContext stratCommutativitySpec |> (evalCondOnTerm f .*. (lar .*. layerLeftMost (liftToContext ruleCommutativity) .*. la)) |> failS


stratRuleTopLayerMany, stratMultiLayerManyROtd :: (IsStrategy f, Navigator a, HasId (f a)) => f a -> LabeledStrategy a
stratRuleTopLayerMany f = label d $ s
    where
        d = "Multilayer Many - " ++ showId f
        s = fix $ \x -> f .*. (layerMany x)

stratMultiLayerManyROtd r = label desc strat
    where
        desc  = "Layer Many Repeat Oncetd - " ++ showId r
        strat = repeatS (oncetd (stratRuleTopLayerMany r)) 

stratMultiDoubleNot, stratDoubleNotUnary, stratDoubleNot, stratLayerDoubleNot, stratLayerTFRuleNotTF, stratLayerUnary, stratDerivDeMorgan, stratDerivLayerDeMorgan:: LabeledStrategy (Context SLogic)
stratMultiDoubleNot = label d s
    where
        d = "Rewrite Strategy Multi Double Not"
        r = liftToContext ruleDoubleNot
        s = evalCondOnTerm isMultiDoubleNot .*. repeat1 r

stratDoubleNotUnary   = label "Rewrite Strategy Unary Double Not" $ ruleMultiDoubleNot |> (liftToContext ruleDoubleNot)
stratLayerDoubleNot   = label "Rewrite Strategy Layered Double Not" ((evalCondOnTerm (not . isUnaryTerm)) .*. layerSome stratDoubleNotUnary)
stratDoubleNot        = label "Rewrite Strategy Layered Double Not" $ stratDoubleNotUnary |> stratLayerDoubleNot 
stratLayerTFRuleNotTF = label "Rewrite Strategy Layered T-Rule Not F or F-RuleNot-T" (c .*. s)
    where
        c = evalCondOnTerm (not . isUnaryTerm) 
        s = layerSome (liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT)   
stratLayerUnary = label "Rewrite Strategy Layered Unary" (c .*. s)
    where
        c = evalCondOnTerm (not . isUnaryTerm) 
        s = layerSome (replicateS 2 (stratDoubleNotUnary .|. liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT))


stratDerivDeMorgan    = label "Rewrite Strategy DeMorgan Derivative"     $ liftToContext stratDeMorgan .*. try(stratDoubleNot)
stratDerivLayerDeMorgan    = label "Rewrite Strategy DeMorgan Derivative"    $ stratRuleTopLayerMany stratDerivDeMorgan


ruleDoubleNotC :: Rule (Context SLogic)
ruleMultiDoubleNot  = convertToRule "Multi Double Not"                "coarsegrain.doublenot"   stratMultiDoubleNot
ruleLayerDoubleNot  = convertToRule "Layered Double Not"              "layer.doublenot" stratLayerDoubleNot
ruleDoubleNotC      = convertToRule "Double Not (All Variants)"       "doublenot.all"     stratDoubleNot


--stratNegTerms :: LabeledStrategy (Context SLogic)
--stratNegTerms = label "Negate term strategy" $ ((check isNot) .*. ((s1 .*. dn) .|. (stratDeMorgan .*. dn))) 
--    where 
--        s1 = layerFirst (stratFRuleComplementA)-- .|. stratTRuleComplementA .|. stratFRuleConjunctionA .|. stratTRuleConjunctionA .|. 
--             --stratFRuleDisjunctionA .|. stratTRuleDisjunctionA .|. ruleIdempotency .|. ruleImplicationElimination .|. ruleEquivalenceElimination)
--        dn = try (layerAll (liftToContext ruleDoubleNot))

--stratDeMorganDoubleNot = (check isNot)

    {--
multiStrategyChoice, multiStrategyRuleOrElse, multiStrategySeq :: [Strategy a] -> LabeledStrategy (Context (a))
multiStrategyChoice xs = label d s
    where
        d = intercalate "-or-" (map showId xs)
        s = choice (map liftToContext xs)

-- Apply of a given list of rules (orelse)
multiStrategyRuleOrElse xs = label d s
    where
        d = intercalate "-orelse-" (map showId xs)
        s = orelse (map liftToContext xs)
-- Apply of a given list of rules (sequence)
multiStrategySeq xs = label d s
    where
        d = intercalate "-and-" (map showId xs)
        s = Combinators.sequence (map xs)

--}


--------------------------------------------------------------------------------------------------------------------------------------
-- Visits -- replica of Traversal.hs
--------------------------------------------------------------------------------------------------------------------------------------
data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany | VisitLeftMost | VisitRightMost

visit :: (Navigator a, IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit v next s = fix $ \a ->
   case v of
      VisitFirst     -> s  |> next .*. a
      VisitOne       -> s .|. next .*. a
      VisitSome      -> s .*. try (next .*. visit VisitMany next s) .|. next .*. a
      VisitAll       -> s .*. (notS next |> (next .*. a))
      VisitMany      -> try s .*. (notS next |> (next .*. a))

visitm :: (Navigator a, IsStrategy f) => Visit -> f a -> Strategy a
visitm v s = fix $ \a ->
   case v of
      VisitLeftMost  -> (check (not.hasLeft) .*. s) |> (ruleLeft .*. a)
      VisitRightMost -> (check (not.hasRight) .*. s) |> (ruleRight .*. a)

--------------------------------------------------------------------------------------------------------------------------------------
-- One-layer visits
--------------------------------------------------------------------------------------------------------------------------------------
layer :: (Navigator a) => Strategy a -> Strategy a
layer s          = ruleDown .*. s .*. ruleUp

layerAll, layerFirst, layerLeftMost, layerRightMost, layerMany :: (IsStrategy f, Navigator a) => f a -> Strategy a
layerAll s       = layer (visit  VisitAll       ruleRight s)
layerSome s      = layer (visit  VisitSome      ruleRight s)
layerFirst s     = layer (visit  VisitFirst     ruleRight s)
layerLeftMost s  = layer (visitm VisitLeftMost  s)
layerRightMost s = layer (visitm VisitRightMost s)
layerMany s      = layer (visit  VisitMany      ruleRight s)

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
-- Apply of a given list of rules (choice)
multiRuleChoice, multiRuleOrElse, multiRuleSeq :: [Rule SLogic] -> LabeledStrategy (Context SLogic)
multiRuleChoice xs = label d s
    where
        d = intercalate "-or-" (map showId xs)
        s = choice (map liftToContext xs)

-- Apply of a given list of rules (orelse)
multiRuleOrElse xs = label d s
    where
        d = intercalate "-orelse-" (map showId xs)
        s = orelse (map liftToContext xs)

-- Apply of a given list of rules (sequence)
multiRuleSeq xs = label d s
    where
        d = intercalate "-and-" (map showId xs)
        s = Combinators.sequence (map liftToContext xs)

-- Apply a rule once somewhere
ruleOnce, ruleAll :: Rule SLogic -> LabeledStrategy (Context SLogic)
ruleOnce r = label ("Rewrite Strategy Single Rule" ++ showId r) (somewhere (liftToContext r))

-- Apply a rule multiple times somewhere repeated
ruleAll r = label ("Rewrite Strategy Multiple Rule" ++ showId r) (repeatS (ruleOnce r))

-- Commutative version of a rule
stratRuleC :: Rule SLogic -> LabeledStrategy (SLogic)
stratRuleC r = label ("Rewrite Strategy Commutative Rule" ++ showId r) (ruleCommutativity .*. r)

stratRuleOnceC, stratRuleAllC :: Rule SLogic -> LabeledStrategy (Context SLogic)
-- Commutative version of a rule
stratRuleOnceC r = label ("rewrite.commutative." ++ showId r) (ruleOnce ruleCommutativity .*. liftToContext r)

-- Commutative version of a rule
stratRuleAllC r = label ("rewrite.commutative." ++ showId r) (ruleAll ruleCommutativity .*. liftToContext r)

--------------------------------------------------------------------------------------------------------------------------------------
-- Set of simple logic strategies and rules, including
-- - Ordered commutativity
-- - DeMorgan
-- - Commutative variants of the rules complement, conjunction, disjunction, absorption and the TT and FF rules are also allowed.
--------------------------------------------------------------------------------------------------------------------------------------
stratFRuleComplementC, stratFRuleConjunctionC, stratTRuleConjunctionC, stratTRuleComplementC,
    stratFRuleDisjunctionC, stratTRuleDisjunctionC, stratCommutativityOrd, stratDeMorgan, stratFRuleComplementA,
    stratTRuleComplementA, stratFRuleConjunctionA, stratTRuleConjunctionA, stratFRuleDisjunctionA, stratTRuleDisjunctionA :: LabeledStrategy SLogic
stratFRuleComplementC  = label "Rewrite Strategy Commutative-and-F-Rule Complement"  $ (check (not . isOrdered) .*. stratRuleC ruleFRuleComplement)  |> failS
stratTRuleComplementC  = label "Rewrite Strategy Commutative-and-T-Rule Complement"  $ (check (not . isOrdered) .*. stratRuleC ruleTRuleComplement)  |> failS
stratFRuleConjunctionC = label "Rewrite Strategy Commutative-and-F-Rule Conjunction" $ (check (not . isOrdered) .*. stratRuleC ruleFRuleConjunction) |> failS
stratTRuleConjunctionC = label "Rewrite Strategy Commutative-and-T-Rule Conjunction" $ (check (not . isOrdered) .*. stratRuleC ruleTRuleConjunction) |> failS


stratFRuleDisjunctionC = label "Rewrite Strategy Commutative-and-F-Rule Disjunction" $ (check (not . isOrdered) .*. stratRuleC ruleFRuleDisjunction) |> failS
stratTRuleDisjunctionC = label "Rewrite Strategy Commutative-and-T-Rule Disjunction" $ (check (not . isOrdered) .*. stratRuleC ruleTRuleDisjunction) |> failS

stratCommutativityOrd  = label "Rewrite Strategy Commutativity-Ordered"              $ (check (not . isOrdered) .*. ruleCommutativity)               |> failS

stratDeMorgan          = label "Rewrite Strategy DeMorgan"                           $ ruleDeMorganOr           |>  ruleDeMorganAnd
stratFRuleComplementA  = label "Rewrite Strategy F-Rule Complement"                  $ ruleFRuleComplement      |>  ruleFRuleComplementC
stratTRuleComplementA  = label "Rewrite Strategy T-Rule Complement"                  $ ruleTRuleComplement      |>  ruleTRuleComplementC
stratFRuleConjunctionA = label "Rewrite Strategy F-Rule Conjunction"                 $ ruleFRuleConjunction     |>  ruleFRuleConjunctionC
stratTRuleConjunctionA = label "Rewrite Strategy T-Rule Conjunction"                 $ ruleTRuleConjunction     |>  ruleTRuleConjunctionC
stratFRuleDisjunctionA = label "Rewrite Strategy F-Rule Disjunction"                 $ ruleFRuleDisjunction     .|. ruleFRuleDisjunctionC
stratTRuleDisjunctionA = label "Rewrite Strategy T-Rule Disjunction"                 $ ruleTRuleDisjunction     .|. ruleTRuleDisjunctionC

ruleFRuleConjunctionC, ruleTRuleConjunctionC, ruleFRuleComplementC, ruleTRuleComplementC, ruleFRuleDisjunctionC, 
    ruleTRuleDisjunctionC, ruleCommutativityOrd, ruleDeMorgan, ruleFRuleComplementA, ruleTRuleComplementA, ruleFRuleConjunctionA, 
    ruleTRuleConjunctionA, ruleFRuleDisjunctionA, ruleTRuleDisjunctionA  :: Rule SLogic
ruleFRuleConjunctionC = convertToRule "Commutative F-Rule Conjunction"    "single.commutativity.and.fruleconjunction" stratFRuleConjunctionC
ruleTRuleConjunctionC = convertToRule "Commutative T-Rule Conjunction"    "single.commutativity.and.trulecomplement"  stratTRuleConjunctionC
ruleFRuleComplementC  = convertToRule "Commutative F-Rule Complement"     "single.commutativity.and.frulecomplement"  stratFRuleComplementC
ruleTRuleComplementC  = convertToRule "Commutative T-Rule Complement"     "single.commutativity.and.trulecomplement"  stratTRuleComplementC
ruleFRuleDisjunctionC = convertToRule "Commutative F-Rule Disjunction"    "single.commutativity.and.fruledisjunction" stratFRuleDisjunctionC
ruleTRuleDisjunctionC = convertToRule "Commutative T-Rule Disjunction"    "single.commutativity.and.truledisjunction" stratTRuleDisjunctionC

ruleCommutativityOrd  = convertToRule "Commutativity (Ordered variant)"   "single.commutativity.ordered"              stratCommutativityOrd

ruleDeMorgan          = convertToRule "De Morgan (And / Or)"              "single.demorgan"                           stratDeMorgan
ruleFRuleComplementA  = convertToRule "F-Rule Complement (All variants)"  "fcomplement.all"                           stratFRuleComplementA
ruleTRuleComplementA  = convertToRule "T-Rule Complement (All variants)"  "tcomplement.all"                           stratTRuleComplementA
ruleFRuleConjunctionA = convertToRule "F-Rule Conjunction (All variants)" "fconjunction.all"                          stratFRuleConjunctionA
ruleTRuleConjunctionA = convertToRule "T-Rule Conjunction (All variants)" "tconjunction.all"                          stratTRuleConjunctionA
ruleFRuleDisjunctionA = convertToRule "F-Rule Disjunction (All variants)" "fruledisjunction.all"                      stratFRuleDisjunctionA
ruleTRuleDisjunctionA = convertToRule "T-Rule Disjunction (All variants)" "truledisjunction.all"                      stratTRuleDisjunctionA

--------------------------------------------------------------------------------------------------------------------------------------
-- Set of advanced logic Strategies and rules, including:
-- - Commutative variant of rules absorption
-- - Generalisations of the rules DeMorgan
--------------------------------------------------------------------------------------------------------------------------------------
isCommutativeAbsorption1, isCommutativeAbsorption2, isCommutativeAbsorption3, isCommutativeAbsorption4, 
    isCommutativeAbsorption5 :: SLogic -> Bool 
isCommutativeAbsorption1 ((p :&&: q) :||: r) | p == r = True
isCommutativeAbsorption1 _                            = False

isCommutativeAbsorption2 (p :||: (q :&&: r)) | p == r = True 
isCommutativeAbsorption2 ((p :||: q) :&&: r) | p == r = True
isCommutativeAbsorption2 _                            = False

isCommutativeAbsorption3 (p :&&: (q :||: r)) | p == r = True--
isCommutativeAbsorption3 _                            = False

isCommutativeAbsorption4 (p :||: (q :&&: r)) | p == q = True 
isCommutativeAbsorption4 _                            = False

isCommutativeAbsorption5 ((p :||: q) :&&: r) | q == r = True
isCommutativeAbsorption5 _                            = False

stratAbsorptionC, stratAbsorptionA, stratDeMorganAndG, stratDeMorganOrG, stratDeMorganG :: LabeledStrategy (Context SLogic)
stratAbsorptionC  = label "Rewrite Strategy Commutativity-Absortion"      $ s
    where
        la  = liftToContext ruleAbsorption
        lc  = liftToContext ruleCommutativity
        hlm = (layerLeftMost lc)  .*. la
        hrm = (layerRightMost lc) .*. la
        s   = (evalCondOnTerm isCommutativeAbsorption1 .*. hlm )         |>   
              (evalCondOnTerm isCommutativeAbsorption2 .*. (lc .*. la))  |> 
              (evalCondOnTerm isCommutativeAbsorption3 .*. hrm )         |>  
              (evalCondOnTerm isCommutativeAbsorption4 .*. (lc .*. hlm)) |> 
              (evalCondOnTerm isCommutativeAbsorption5 .*. (lc .*. hrm)) |> 
              failS

stratAbsorptionA  = label "Rewrite Strategy Commutativity-Absortion"     $ ruleToStrategy (ruleAbsorption) |> ruleAbsorptionC
stratDeMorganAndG = label "Rewrite Strategy DeMorgan And Generalisation" $ stratRuleTopLayerMany (ruleToStrategy ruleDeMorganAnd)
stratDeMorganOrG  = label "Rewrite Strategy DeMorgan Or Generalisation"  $ stratRuleTopLayerMany (ruleToStrategy ruleDeMorganOr)
stratDeMorganG    = label "Rewrite Strategy DeMorgan Generalisations"    $ stratRuleTopLayerMany (ruleToStrategy ruleDeMorgan)
stratDeMorganA    = label "Rewrite Strategy DeMorgan All Variants)"      $ liftToContext stratDeMorgan |> stratDeMorganG

ruleAbsorptionC, ruleAbsorptionA, ruleDeMorganAndG, ruleDeMorganOrG, ruleDeMorganG, ruleDeMorganA :: Rule (Context SLogic)
ruleAbsorptionC  = convertToRule "Commutative Absorption"                "single.commutativity.absorption" stratAbsorptionC
ruleAbsorptionA  = convertToRule "Commutative Absorption (All variants)" "absorption.all"                  stratAbsorptionA
ruleDeMorganAndG = convertToRule "DeMorgan And Generalisations"          "generalisations.DeMorgan.And"    stratDeMorganAndG
ruleDeMorganOrG  = convertToRule "DeMorgan Or Generalisations"           "generalisations.DeMorgan.Or"     stratDeMorganOrG
ruleDeMorganG    = convertToRule "DeMorgan Generalisations"              "generalisations.DeMorgan"        stratDeMorganG
ruleDeMorganA    = convertToRule "DeMorgan (All variants)"               "DeMorgan.all"                    stratDeMorganA

--------------------------------------------------------------------------------------------------------------------------------------
-- Set of advanced logic Strategies, including:
-- - Derivative variants of rules DeMorgan (To be reviewed - needs extension)
-------------------------------------------------------------------------------------------------------------------------------------
deMorganComplete, multiDeMorgan, deMorganDeriv, deMorganDeriv1, deMorganDeriv2, deMorganDeriv3, deMorganDeriv4,
    negation, multiNegation, multiDoubleNot, implicationEliminationDeriv1, implicationEliminationDeriv2, implicationEliminationDeriv3,
    implicationEliminationDeriv4, multiImplicationElimination, implicationEliminationDeriv, implicationEliminationComplete,
    mulitImplicationEliminationDeriv, multiDeMorganDeriv :: LabeledStrategy (Context SLogic)
--deMorgan (Not (p :&&: T)) = Just (Not p :||: Not T) = Just (Not p :||: F) = Just Not p
--deMorgan (Not (T :&&: p)) = Just (Not T :||: Not p) = Just (F :||: Not p) = Just Not p
--deMorganDeriv1 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction
deMorganDeriv1 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction
--deMorganDeriv1 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction .*. multiNegation
--deMorganDeriv1 = label "DeMorgan Derivative 1" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :&&: F)) = Just (Not p :||: Not F) = Just (Not p :||: T) = Just T
--deMorgan (Not (F :&&: p)) = Just (Not F :||: Not p) = Just (T :||: Not p) = Just T
deMorganDeriv2 = label "DeMorgan Derivative" $  innermost (liftToContext ruleDeMorgan) .*. oncetdPref (liftToContext ruleTRuleNotF)-- .*. liftToContext ruleTRuleConjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction .*. multiNegation
--deMorganDeriv2 = label "DeMorgan Derivative 2" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :||: T)) = Just (Not p :&&: Not T) = Just (Not p :&&: F) = Just F
--deMorgan (Not (T :||: p)) = Just (Not T :&&: Not p) = Just (F :&&: Not p) = Just F
deMorganDeriv3 = label "DeMorgan Derivative" $  liftToContext ruleDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction
--deMorganDeriv3 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction
--deMorganDeriv3 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction .*. multiNegation
--deMorganDeriv3 = label "DeMorgan Derivative 3" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :||: F)) = Just (Not p :&&: Not F) = Just (Not p :&&: T) = Just Not p
--deMorgan (Not (F :||: p)) = Just (Not F :&&: Not p) = Just (T :&&: Not p) = Just Not p
deMorganDeriv4 = label "DeMorgan Derivative" $  liftToContext ruleDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction
--deMorganDeriv4 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction
--deMorganDeriv4 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction .*. multiNegation
--deMorganDeriv4 = label "DeMorgan Derivative 4" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction .*. multiNegation .*. multiDoubleNot

--------------------------------------------------------------------------------------------------------------------------------------
-- Legacy Code - to be reviewed
--------------------------------------------------------------------------------------------------------------------------------------
negation = label "Negate" $ multiRuleChoice [ruleTRuleNotF, ruleFRuleNotT]
multiDeMorgan = label "Multi DeMorgan" $ repeatS (somewhere (liftToContext ruleDeMorgan))

--ruleDeMorgan :: Ord a => Eq a => LgcRule a
--ruleDeMorgan = convertToRule "De Morgan" "single.demorgan" deMorgan

multiNegation = label "Multi Negate" $ repeatS (somewhere negation)
multiDoubleNot = ruleAll ruleDoubleNot

deMorganDeriv = label "DeMorgan Derivative" $ deMorganDeriv1 .|. deMorganDeriv2 .|. deMorganDeriv3 .|. deMorganDeriv4
multiDeMorganDeriv = label "Multi DeMorgan Derivative" $ repeatS (somewhere deMorganDeriv)

deMorganComplete = label "Complete DeMorgan" $ deMorganDeriv |> multiDeMorganDeriv |> multiDeMorgan

-- create new rule (draft)
-- ruleDeMorganComplete = convertToRule "DeMorgan Complete" "demorgan.complete" deMorganComplete

testlf, testlta, testlta2 :: Rule SLogic -> LabeledStrategy (Context SLogic)
--testlf = label "layered first" $  layerFirst (liftToContext ruleDoubleNot)
testlf x = label description strategy
    where
        description = "Layered First " ++ showId x
        strategy = layer (visit VisitFirst ruleRight (liftToContext x))

testlta x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = liftToContext x .*. layer (visit VisitFirst ruleRight (liftToContext x))

testlta2 x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = repeatS (liftToContext x .*. layer (visit VisitFirst ruleRight (liftToContext x)))

testlta3, testlmo :: LabeledStrategy (Context SLogic) -> LabeledStrategy (Context SLogic)
testlta3 x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = x .*. layer (visit VisitAll ruleRight x)

testlmo x = label description strategy
    where
        description = "Layered Left Most Only " ++ showId x
        strategy = x .*. layer (visitm VisitLeftMost x)


multiImplicationElimination = label "Multi Implication Elimination" $ repeatS (somewhere (liftToContext ruleImplicationElimination))
mulitImplicationEliminationDeriv = label "Multi Implication Elimination Derivative" $  repeatS (somewhere implicationEliminationDeriv)
implicationEliminationDeriv = label "Implication Elimination Derivative" $ implicationEliminationDeriv1 .|. implicationEliminationDeriv2 .|. implicationEliminationDeriv3 
                                                                            .|. implicationEliminationDeriv4
implicationEliminationComplete = label "Complete Implication Elimination" $ implicationEliminationDeriv |> mulitImplicationEliminationDeriv 
                                                                            |> (liftToContext ruleImplicationElimination .|. multiDeMorgan)

-- implicationEliminationRule (T :->: p) = Not T :||: p = F :||: p = Just p
implicationEliminationDeriv1 = label "Implication Elimination Derivative 1" $ liftToContext ruleImplicationElimination .*. oncetdPref (liftToContext ruleFRuleNotT) 
                                                                                .*. liftToContext ruleFRuleDisjunction

-- implicationEliminationRule (F :->: p) = Not F :||: p = T :||: p = Just T
implicationEliminationDeriv2 = label "Implication Elimination Derivative 2" $ liftToContext ruleImplicationElimination .*. oncetdPref (liftToContext ruleTRuleNotF) 
                                                                                .*. liftToContext ruleTRuleDisjunction

-- implicationEliminationRule (p :->: T) = Not p :||: T = Not p :||: T = Just T                                                                                
implicationEliminationDeriv3 = label "Implication Elimination Derivative 3" $ liftToContext ruleImplicationElimination .*. oncetdPref (liftToContext ruleTRuleDisjunction)

-- implicationEliminationRule (p :->: F) = Not p :||: F = Not p :||: F = Just Not p
implicationEliminationDeriv4 = label "Implication Elimination Derivative 4" $ liftToContext ruleImplicationElimination .*. oncetdPref (liftToContext ruleFRuleDisjunction)

{--
equivalenceElimination (T :<->: p) = Just ((T :&&: p) :||: (Not T :&&: Not p)) = Just ((T :&&: p) :||: (F :&&: Not p)) = Just (p :||: (F :&&: Not p)) = Just p :||: F = Just p
equivalenceElimination1 = label "Equivalence Elimination Derivative 1" $ liftToContext ruleEquivalenceElimination .*. oncetdPref (liftToContext ruleFRuleNotT) 

equivalenceElimination (F :<->: p) = Just ((F :&&: p) :||: (Not F :&&: Not p)) = Just (F :||: (Not F :&&: Not p)) = Just (F :||: (T :&&: Not p)) = Just F :||: Not p = Just Not p
equivalenceElimination2 = label "Equivalence Elimination Derivative 2" $ liftToContext ruleEquivalenceElimination .*. oncetdPref (liftToContext ruleFRuleNotT) 

equivalenceElimination (p :<->: T) = Just ((p :&&: T) :||: (Not p :&&: Not T)) = Just (p :||: (Not p :&&: Not T)) = Just (p :||: (Not p :&&: F)) = Just p :||: F = Just p
equivalenceElimination3 = label "Equivalence Elimination Derivative 3" $ liftToContext ruleEquivalenceElimination .*. oncetdPref (liftToContext ruleFRuleNotT) 

equivalenceElimination (p :<->: F) = Just ((p :&&: F) :||: (Not p :&&: Not F)) = Just (F :||: (Not p :&&: Not F)) = Just (F :||: (Not p :&&: T)) = Just F :||: Not p = Just Not p
equivalenceElimination4 = label "Equivalence Elimination Derivative 4" $ liftToContext ruleEquivalenceElimination .*. oncetdPref (liftToContext ruleFRuleNotT) 
--}

----------------
-- Legacy code
----------------


evalStrategy' :: LabeledStrategy SLogic -> LabeledStrategy (Context SLogic)
evalStrategy' x = label "eval" $ repeatS (somewhere (liftToContext x))


evalStrategy2 :: LabeledStrategy SLogic -> LabeledStrategy (Context SLogic)
evalStrategy2 x = label "eval2" $ repeat1 (somewhere (liftToContext x))

--evalStrategy3 :: LabeledStrategy (SLogic) -> LabeledStrategy (Context (SLogic))
--evalStrategy3 x = label "eval3" $ somewhere (liftToContext x) .*. evalStrategy x

--evalStrategy4 :: LabeledStrategy (SLogic) -> LabeledStrategy (Context (SLogic))
--evalStrategy4 x = label "eval4" $ somewhere (liftToContext x) |> evalStrategy x

--evalStrategy5 ::  LabeledStrategy (SLogic) -> LabeledStrategy (SLogic) -> LabeledStrategy (Context (SLogic))
--evalStrategy5 x y = label "eval5" $ somewhere (liftToContext x) .|. evalStrategy y

--evalStrategy6 :: LabeledStrategy (SLogic) -> LabeledStrategy (Context (SLogic))
--evalStrategy6 x = label "eval6" $ somewhere (liftToContext x)


--evalStrategy5 :: [ LSExpr a ] -> LSContext a
--evalStrategy5 x = label "eval5" $ strategy
--    where 
--        functions = map (\y -> somewhere (liftToContext y)) x
--        strategy = map (|>) functions
{--
multiRuleChoice :: [ LogicRule a ] -> LabeledStrategy (Context (Expr a) -> Strategy (Expr a))
multiRuleChoice x = label description $ result
	where
		result = map (.|.) x
		descriptions = map (snd.findRedRuleDescription) 
		description = concat (intersperse "-or-" descriptions)
choiceStrategy :: Eq a => [String] -- -> LabeledStrategy (g0 (LogicRule a) -> Strategy (LogicRule a))
choiceStrategy x = label description -- $ strategy
    where
        description = intercalate "-or-" x
        -- strategy = map ((.|.).getRedRule) x
        --description = snd ( findRedRuleDescription ruleName1) ++ "-or-" ++ snd ( findRedRuleDescription ruleName2)


-- doubleNot :: LSExpr a
-- doubleNot = label "doublenot" $ createRule doubleNotStd .|. createRule doubleNotDeriv
doubleNot :: Ord a => Eq a => LSLgc a
doubleNot = label "doublenot" ruleDoubleNot

doubleNotOrDeMorgan :: Ord a => Eq a => LSLgc a
doubleNotOrDeMorgan = label "doublenot-or-demorgan" $ ruleDoubleNot .|. ruleDeMorganAnd .|. ruleDeMorganOr

deMorganAndNextDoubleNot :: Ord a => Eq a => LSLgc a
deMorganAndNextDoubleNot = label "demorgan-and-next-doublenot" $ ruleDeMorganAnd .|. ruleDeMorganOr .*. ruleDoubleNot

deMorganInterleaveDoubleNot :: Ord a => Eq a => LSLgc a
deMorganInterleaveDoubleNot = label "demorgan-Interleave-doublenot" $ ruleDeMorganAnd .|. ruleDeMorganOr .%. ruleDoubleNot

doubleNotOrImplication :: Eq a => LSExpr a
doubleNotOrImplication = label "doublenot-or-implication" $ doubleNotRule .|. implicationRule

doubleNotOrLogicalEquivalence :: Eq a => LSExpr a
doubleNotOrLogicalEquivalence = label "doublenot-or-logicalequivalence" $ doubleNotRule .|. logicalEquivalenceRule

doubleNotOrAbsorption :: Eq a => Eq a => LSExpr a
doubleNotOrAbsorption = label "doublenot-or-absorption" $ doubleNotRule .|. absorptionRule

doubleNotOrIdempotency :: Eq a => Eq a => LSExpr a
doubleNotOrIdempotency = label "doublenot-or-idempotency" $ doubleNotRule .|. idempotencyRule

doubleNotOrFRuleConjunction :: Eq a => LSExpr a
doubleNotOrFRuleConjunction = label "doublenot-or-fruleconjunction" $ doubleNotRule .|. fRuleConjunctionRule

doubleNotOrTRuleConjunction :: Eq a => LSExpr a
doubleNotOrTRuleConjunction = label "doublenot-or-truleconjunction" $ doubleNotRule .|. tRuleConjunctionRule

doubleNotOrFRuleDisjunction :: Eq a => LSExpr a
doubleNotOrFRuleDisjunction = label "doublenot-or-fruledisjunction" $ doubleNotRule .|. fRuleDisjunctionRule

doubleNotOrTRuleDisjunction :: Eq a => LSExpr a
doubleNotOrTRuleDisjunction = label "doublenot-or-truledisjunction" $ doubleNotRule .|. tRuleDisjunctionRule

doubleNotOrTRuleComplement :: Eq a => Eq a => LSExpr a
doubleNotOrTRuleComplement = label "doublenot-or-trulecomplement" $ doubleNotRule .|. tRuleComplementRule

doubleNotOrFRuleComplement :: Eq a => Eq a => LSExpr a
doubleNotOrFRuleComplement = label "doublenot-or-frulecomplement" $ doubleNotRule .|. fRuleComplementRule

doubleNotOrTRuleNot :: Eq a => LSExpr a
doubleNotOrTRuleNot = label "doublenot-or-trulenot" $ doubleNotRule .|. tRuleNotRule

doubleNotOrFRuleNot :: Eq a => LSExpr a
doubleNotOrFRuleNot = label "doublenot-or-frulenot" $ doubleNotRule .|. fRuleNotRule

deMorganOrImplication :: Eq a => LSExpr a
deMorganOrImplication = label "demorgan-or-implication" $ deMorganRule .|. implicationRule

deMorganOrLogicalEquivalence :: Eq a => LSExpr a
deMorganOrLogicalEquivalence = label "demorgan-or-implication" $ deMorganRule .|. logicalEquivalenceRule 

deMorganOrAbsorption :: Eq a => LSExpr a
deMorganOrAbsorption = label "demorgan-or-absorption" $ deMorganRule .|. absorptionRule

deMorganOrIdempotency :: Eq a => LSExpr a
deMorganOrIdempotency = label "demorgan-or-idempotency" $ deMorganRule .|. idempotencyRule

deMorganOrFRuleConjunction :: Eq a => LSExpr a
deMorganOrFRuleConjunction = label "demorgan-or-fruleconjunction" $ deMorganRule .|. fRuleConjunctionRule

deMorganOrTRuleConjunction :: Eq a => LSExpr a
deMorganOrTRuleConjunction = label "demorgan-or-truleconjunction" $ deMorganRule .|. tRuleConjunctionRule

deMorganOrFRuleDisjunction :: Eq a => LSExpr a
deMorganOrFRuleDisjunction = label "demorgan-or-fruledisjunction" $ deMorganRule .|. fRuleDisjunctionRule

deMorganOrTRuleDisjunction :: Eq a => LSExpr a
deMorganOrTRuleDisjunction = label "demorgan-or-truledisjunction" $ deMorganRule .|. tRuleDisjunctionRule

deMorganOrTRuleComplement :: Eq a => Eq a => LSExpr a
deMorganOrTRuleComplement = label "demorgan-or-trulecomplement" $ deMorganRule .|. tRuleComplementRule

deMorganOrFRuleComplement :: Eq a => Eq a => LSExpr a
deMorganOrFRuleComplement = label "demorgan-or-frulecomplement" $ deMorganRule .|. fRuleComplementRule

doubleNotOrDeMorganOrImplication :: Eq a => LSExpr a
doubleNotOrDeMorganOrImplication = label "doublenot-or-demorgan-or-implication" $ doubleNotRule .|. deMorganRule .|. implicationRule

doubleNotOrDeMorganOrLogicalEquivalence :: Eq a => LSExpr a
doubleNotOrDeMorganOrLogicalEquivalence = label "doublenot-or-demorgan-or-logicalequivalence" $ deMorganRule .|. logicalEquivalenceRule 

doubleNotOrdeMorganOrAbsorption :: Eq a => LSExpr a
doubleNotOrdeMorganOrAbsorption = label "doublenot-or-demorgan-or-absorption" $ doubleNotRule .|. deMorganRule .|. absorptionRule

doubleNotOrdeMorganOrIdempotency :: Eq a => LSExpr a
doubleNotOrdeMorganOrIdempotency = label "doublenot-or-demorgan-or-idempotency" $ doubleNotRule .|. deMorganRule .|. idempotencyRule

doubleNotOrdeMorganOrFRuleConjunction :: Eq a => LSExpr a
doubleNotOrdeMorganOrFRuleConjunction = label "doublenot-or-demorgan-or-fruleconjunction" $ doubleNotRule .|. deMorganRule .|. fRuleConjunctionRule

doubleNotOrdeMorganOrTRuleConjunction :: Eq a => LSExpr a
doubleNotOrdeMorganOrTRuleConjunction = label "doublenot-or-demorgan-or-truleconjunction" $ doubleNotRule .|. deMorganRule .|. tRuleConjunctionRule

doubleNotOrdeMorganOrFRuleDisjunction :: Eq a => LSExpr a
doubleNotOrdeMorganOrFRuleDisjunction = label "doublenot-or-demorgan-or-fruledisjunction" $ doubleNotRule .|. deMorganRule .|. fRuleDisjunctionRule

doubleNotOrdeMorganOrTRuleDisjunction :: Eq a => LSExpr a
doubleNotOrdeMorganOrTRuleDisjunction = label "doublenot-or-demorgan-or-truledisjunction" $ doubleNotRule .|. deMorganRule .|. tRuleDisjunctionRule

doubleNotOrdeMorganOrTRuleComplement :: Eq a => Eq a => LSExpr a
doubleNotOrdeMorganOrTRuleComplement = label "doublenot-or-demorgan-or-trulecomplement" $ doubleNotRule .|. deMorganRule .|. tRuleComplementRule

doubleNotOrdeMorganOrFRuleComplement :: Eq a => Eq a => LSExpr a
doubleNotOrdeMorganOrFRuleComplement = label "doublenot-or-demorgan-or-frulecomplement" $ doubleNotRule .|. deMorganRule .|. fRuleComplementRule



       --}
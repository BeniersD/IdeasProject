{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicReductionStrategies where

import Data.List
import Data.Maybe
import Domain.Logic.Formula 
import qualified Ideas.Common.Strategy.Combinators as Combinators
import Ideas.Common.Library hiding (layer, isUnary)
import Ideas.Common.Traversal.Navigator
import Ideas.Utils.Uniplate
import LogicFunctions
import LogicReductionRules

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Data types, classes, generic Strategies
-------------------------------------------------------------------------------------------------------------------------------------------------
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

-- Check function with context 
evalCondOnTerm :: (SLogic -> Bool) -> Strategy (Context SLogic)
evalCondOnTerm c = check (maybe False c . currentInContext)      

-- Apply layerMany multiple fimes
stratRuleTopLayerMany :: (IsStrategy f, Navigator a, HasId (f a)) => f a -> LabeledStrategy a
stratRuleTopLayerMany f = label d $ s
    where
        d = "Multilayer Many - " ++ showId f
        s = fix $ \x -> f .*. (layerMany x)

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

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Visits -- replica of Traversal.hs
-------------------------------------------------------------------------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------------------------------------------------------------------------
-- One-layer visits -- replica of Traversal.hs
-------------------------------------------------------------------------------------------------------------------------------------------------
layer :: (Navigator a) => Strategy a -> Strategy a
layer s          = ruleDown .*. s .*. ruleUp

layerAll, layerFirst, layerLeftMost, layerRightMost, layerMany :: (IsStrategy f, Navigator a) => f a -> Strategy a
layerAll s       = layer (visit  VisitAll       ruleRight s)
layerSome s      = layer (visit  VisitSome      ruleRight s)
layerFirst s     = layer (visit  VisitFirst     ruleRight s)
layerLeftMost s  = layer (visitm VisitLeftMost  s)
layerRightMost s = layer (visitm VisitRightMost s)
layerMany s      = layer (visit  VisitMany      ruleRight s)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Set of logic rewrite strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
ruleFRuleConjunctionC, ruleTRuleConjunctionC, ruleFRuleComplementC, ruleTRuleComplementC, ruleFRuleDisjunctionC, ruleTRuleDisjunctionC, 
    ruleCommutativityOrd, ruleFRuleComplementA, ruleTRuleComplementA, ruleFRuleConjunctionA, ruleTRuleConjunctionA, ruleFRuleDisjunctionA, 
    ruleTRuleDisjunctionA, ruleDeMorgan, ruleAbsorptionC, ruleAbsorptionA, ruleAC, ruleACI, ruleDeMorganAndG, ruleDeMorganOrG, ruleDeMorganG, 
    ruleDeMorganA, ruleDeMorganD, ruleDoubleNotC, ruleMultiDoubleNot, ruleLayerDoubleNot, ruleMultiTFRuleNotTF, ruleLayerTFRuleNotTF, 
    ruleTFRuleNotTFA, ruleImplicationEliminationA, ruleEquivalenceEliminationA, ruleNegations :: Rule (Context SLogic)

stratFRuleComplementC, stratFRuleConjunctionC, stratTRuleConjunctionC, stratTRuleComplementC, stratFRuleDisjunctionC, stratTRuleDisjunctionC, 
    stratCommutativityOrd, stratDeMorgan, stratFRuleComplementA, stratTRuleComplementA, stratFRuleConjunctionA, stratTRuleConjunctionA, 
    stratFRuleDisjunctionA, stratTRuleDisjunctionA, stratAbsorptionC, stratAbsorptionA, stratDeMorganAndG, stratDeMorganOrG, stratDeMorganG, 
    stratDeMorganD, stratMultiDoubleNot,     stratDoubleNotUnary, stratDoubleNot, stratLayerDoubleNot, stratLayerTFRuleNotTF, stratUnairies,  
    stratConjunctions, stratMultiTFRuleNotTF, stratTFRuleNotTFUnary, stratTFRuleNotTFA, stratDisjunctions, stratAbsorbers, 
    stratImplicationEliminationD, stratImplicationEliminationA, stratEquivalenceEliminationD, stratImplicationElimination, 
    stratEquivalenceEliminationA, stratEquivalenceElimination, stratNegations :: LabeledStrategy (Context SLogic)

stratRuleC :: Rule (Context SLogic) -> LabeledStrategy (Context SLogic)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Absorption variants 
-------------------------------------------------------------------------------------------------------------------------------------------------
stratAbsorptionC  = label "Strategy Commutativity-Absortion"                 $ s
    where
        f1, f2, f3, f4, f5 :: SLogic -> Bool 
        f1 ((p :&&: q) :||: r) | p == r = True
        f1 _                            = False

        f2 (p :||: (q :&&: r)) | p == r = True 
        f2 ((p :||: q) :&&: r) | p == r = True
        f2 _                            = False

        f3 (p :&&: (q :||: r)) | p == r = True--
        f3 _                            = False

        f4 (p :||: (q :&&: r)) | p == q = True 
        f4 _                            = False

        f5 ((p :||: q) :&&: r) | q == r = True
        f5 _                            = False

        la  = liftToContext ruleAbsorption
        lc  = liftToContext ruleCommutativity
        hlm = (layerLeftMost lc)  .*. la
        hrm = (layerRightMost lc) .*. la
        s   = (evalCondOnTerm f1 .*. hlm )         |>   
              (evalCondOnTerm f2 .*. (lc .*. la))  |> 
              (evalCondOnTerm f3 .*. hrm )         |>  
              (evalCondOnTerm f4 .*. (lc .*. hlm)) |> 
              (evalCondOnTerm f5 .*. (lc .*. hrm)) 
stratAbsorptionA = label "Strategy Commutativity-Absortion"                  $ s
    where
        s = ruleAbsorptionD |> ruleAbsorptionC |> liftToContext ruleAbsorption 
stratAbsorptionD = label "Strategy  Absortion Derivative"                    $ s
    where    
        f1, f2 :: SLogic -> Bool
        f1 ((Not p :&&: q) :||: r) | p == r           = True
        f1 ((p :&&: Not q) :||: r) | q == r           = True
        f1 ((p :&&: q) :||: Not r) | p == r || q == r = True
        f1 (Not p :||: (q :&&: r)) | p == q || p == r = True
        f1 (p :||: (Not q :&&: r)) | p == q           = True
        f1 (p :||: (q :&&: Not r)) | p == r           = True 
        f1 _                                          = False 

        f2 ((Not p :||: q) :&&: r) | p == r           = True
        f2 ((p :||: Not q) :&&: r) | q == r           = True
        f2 ((p :||: q) :&&: Not r) | p == r || q == r = True 
        f2 (Not p :&&: (q :||: r)) | p == q || p == r = True
        f2 (p :&&: (Not q :||: r)) | p == q           = True 
        f2 (p :&&: (q :||: Not r)) | p == r           = True
        f2 _                                          = False   

        ld = liftToContext ruleDistributivity
        s1 = (evalCondOnTerm f1 .*. ld .*. layerMany (stratTRuleComplementA) .*. stratTRuleConjunctionA) 
        s2 = (evalCondOnTerm f2 .*. ld .*. layerMany (stratFRuleComplementA) .*. stratFRuleDisjunctionA)   
        s  = (s1 |> s2)

ruleAbsorptionA  = convertToRule "Commutative Absorption (All variants)"     "all.absorption"                  stratAbsorptionA
ruleAbsorptionC  = convertToRule "Commutative Absorption"                    "combi.absorption.commutativity"  stratAbsorptionC
ruleAbsorptionD  = convertToRule "Commutative Absorption Derivative"         "combi.absorption.derivative"     stratAbsorptionD

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Associativity variants 
-------------------------------------------------------------------------------------------------------------------------------------------------
ruleassociativityR :: Rule SLogic
ruleassociativityR    = createRule "Associativity Reversed"                  "single.associativity.reversed"          f
    where
        f ::  SLogic -> Maybe (SLogic)
        f (p :||: (q:||: r)) = Just ((p :||: q) :||: r) 
        f (p :&&: (q :&&: r)) = Just ((p :&&: q) :&&: r)
        f _ = Nothing

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Combinated strategies like
-- - Combination of associativity/ commutativity
-- - Combination of associativity/ commutativity and Idempotency  
-- - Combination of unaries
-- - Combination of conjunctions
-- - Combination of disjunctions
-------------------------------------------------------------------------------------------------------------------------------------------------
stratAC, stratACI :: LabeledStrategy (Context SLogic)
stratAC                 = label "Strategy Associativity-Commutativity"       $  s
    where
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :||: q)                             = True
        f (p :||: (q :||: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :||: skipNegations q) = True
        f (p :&&: (q :&&: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :&&: q)                             = True
        f (p :&&: (q :&&: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :&&: skipNegations q) = True
        f _                                                                                                                      = False
        lar = liftToContext ruleassociativityR
        la  = liftToContext ruleAssociativity
        rc = (check (not . isAssoCommOrdered) .*. ruleCommutativity) 
        ra = (liftToContext ruleAssociativity |> liftToContext rc |> 
             (evalCondOnTerm f .*. (lar .*. layerLeftMost (liftToContext ruleCommutativity) .*. la))) 
        s = repeat1 (somewhere ra)
stratACI                = label d                                            $  s
    where
        d = "Strategy Associativity-Commutativity Idempotency"
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) | p == q = True
        f (p :&&: (q :&&: r)) | p == q = True
        f _                            = False
        lar = liftToContext ruleassociativityR
        ri = (liftToContext ruleIdempotency |> (evalCondOnTerm f .*. lar))
        s = (stratAC .*. repeatS( oncebu ri))

stratUnairies           = label d                                            $ stratDoubleNot .|. stratTFRuleNotTFA
    where
        d = "Strategy Double Not / F-Rule Not T / T-Rule Not F"
stratConjunctions       = label d                                            $ s
    where
        d = "Strategy F-Rule Complement / F-Rule Conjunction / T-Rule Conjunction"
        s = (stratFRuleComplementA .|. stratFRuleConjunctionA .|. stratTRuleConjunctionA) 
stratDisjunctions       = label d                                            $ s
    where
        d = "Strategy T-Rule Complement / F-Rule Disjunction / T-Rule Disjunction"
        s = (stratTRuleComplementA .|. stratFRuleDisjunctionA .|. stratTRuleDisjunctionA) 
stratAbsorbers          = label d                                            $ s
    where
        d = "Strategy Absorption / Idempotency"
        s = (stratAbsorptionA .|. liftToContext ruleIdempotency)
stratGenerics           = label d                                            $ s
    where
        d = "SImplication Elimination / Equivalence Elimination / DeMorgan"
        s = (stratImplicationEliminationA .|. stratEquivalenceEliminationA .|. stratDeMorganA)
stratAll                = label d                                            $ s
    where
        d = "Strategy all rules"
        r = (ruleNegations |> stratAbsorbers |> stratConjunctions |> stratDisjunctions |> stratGenerics)
        s = (try (repeat1 (somewhere stratUnairies)) .*. r)
stratNegations = label "Strategy Negate" $ s 
    where 
        c = evalCondOnTerm isNegation
        r = (stratAbsorbers |> stratConjunctions |> stratDisjunctions |> stratGenerics)
        s = (c .*. layerFirst r .*. try (repeat1 (somewhere stratUnairies)))

ruleAC                  = convertToRule "Associativity-Commutativity"        "combi.ac"                                  stratAC  
ruleACI                 = convertToRule d                                    "combi.aci"                                 stratACI    
    where
        d = "Associativity-Commutativity-Idempotency"
ruleNegations           = convertToRule "Negate"                             "combi.negate"                              stratNegations

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Commutativity variants
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Commutative version of a rule
stratRuleC r           = label ("Strategy Commutativy of a Rule " ++ showId r) (liftToContext ruleCommutativity .*. r)
stratCommutativityOrd  = label "Strategy Commutativity-Ordered"              $ (evalCondOnTerm (not . isOrdered) .*. liftToContext ruleCommutativity)    

ruleCommutativityOrd   = convertToRule "Commutativity (Ordered variant)"     "single.commutativity.ordered"              stratCommutativityOrd

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Conjunction variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleConjunctionA = label "Strategy F-Rule Conjunction"                 $ s
    where
        s = (ruleFRuleConjunctionC |> liftToContext ruleFRuleConjunction)  
stratTRuleConjunctionA = label "Strategy T-Rule Conjunction"                 $ s
    where
        s = (ruleTRuleConjunctionC |> liftToContext ruleTRuleConjunction)
stratFRuleConjunctionC = label "Strategy Commutative-and-F-Rule Conjunction" $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleConjunction)) 
    where
        f :: SLogic -> Bool
        f (F :&&: _) = True
        f _          = False  
stratTRuleConjunctionC = label "Strategy Commutative-and-T-Rule Conjunction" $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleConjunction)) 
    where
        f :: SLogic -> Bool
        f (T :&&: _) = True
        f _          = False  
ruleFRuleConjunctionA  = convertToRule "F-Rule Conjunction (All variants)"   "all.fconjunction"                          stratFRuleConjunctionA
ruleTRuleConjunctionA  = convertToRule "T-Rule Conjunction (All variants)"   "all.tconjunction"                          stratTRuleConjunctionA
ruleFRuleConjunctionC  = convertToRule "Commutative F-Rule Conjunction"      "combi.commutativity.and.fruleconjunction"  stratFRuleConjunctionC
ruleTRuleConjunctionC  = convertToRule "Commutative T-Rule Conjunction"      "combi.commutativity.and.trulecomplement"   stratTRuleConjunctionC

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Complement variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleComplementC  = label "Strategy Commutative-and-F-Rule Complement"  $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleComplement)) 
    where
        f :: SLogic -> Bool
        f (Not p :&&: q) | p == q = True
        f _                       = False  
stratTRuleComplementC  = label "Strategy Commutative-and-T-Rule Complement"  $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleComplement))  
    where
        f :: SLogic -> Bool
        f (Not p :||: q) | p == q = True
        f _                       = False 
stratFRuleComplementA  = label "Strategy F-Rule Complement"                  $ (ruleFRuleComplementC |> liftToContext ruleFRuleComplement) 
stratTRuleComplementA  = label "Strategy T-Rule Complement"                  $ (ruleTRuleComplementC |> liftToContext ruleTRuleComplement)   

ruleFRuleComplementC   = convertToRule "Commutative F-Rule Complement"       "combi.commutativity.and.frulecomplement"   stratFRuleComplementC
ruleTRuleComplementC   = convertToRule "Commutative T-Rule Complement"       "combi.commutativity.and.trulecomplement"   stratTRuleComplementC
ruleFRuleComplementA   = convertToRule "F-Rule Complement (All variants)"    "all.fcomplement"                           stratFRuleComplementA
ruleTRuleComplementA   = convertToRule "T-Rule Complement (All variants)"    "all.tcomplement"                           stratTRuleComplementA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Disjunction variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleDisjunctionC = label "Strategy Commutative-and-F-Rule Disjunction" $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleDisjunction)) 
    where
        f :: SLogic -> Bool
        f (F :||: _) = True
        f _          = False        
stratTRuleDisjunctionC = label "Strategy Commutative-and-T-Rule Disjunction" $ (evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleDisjunction))
    where
        f :: SLogic -> Bool
        f (T :||: _) = True
        f _          = False        
stratFRuleDisjunctionA = label "Strategy F-Rule Disjunction"                 $ (ruleFRuleDisjunctionC |> liftToContext ruleFRuleDisjunction) 
stratTRuleDisjunctionA = label "Strategy T-Rule Disjunction"                 $ (ruleTRuleDisjunctionC |> liftToContext ruleTRuleDisjunction) 

ruleFRuleDisjunctionC  = convertToRule "Commutative F-Rule Disjunction"      "combi.commutativity.and.fruledisjunction"  stratFRuleDisjunctionC
ruleTRuleDisjunctionC  = convertToRule "Commutative T-Rule Disjunction"      "combi.commutativity.and.truledisjunction"  stratTRuleDisjunctionC
ruleFRuleDisjunctionA  = convertToRule "F-Rule Disjunction (All variants)"   "all.fruledisjunction"                      stratFRuleDisjunctionA
ruleTRuleDisjunctionA  = convertToRule "T-Rule Disjunction (All variants)"   "all.truledisjunction"                      stratTRuleDisjunctionA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- DeMorgan variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratDeMorgan         = label "Strategy DeMorgan"                            $ liftToContext ruleDeMorganOr .|. liftToContext ruleDeMorganAnd
stratDeMorganA        = label "Strategy DeMorgan All Variants)"              $ s
    where
        s = (ruleDeMorganAndG |> ruleDeMorganOrG |> ruleDeMorganG |> stratDeMorganD |> stratDeMorgan)
stratDeMorganAndG     = label "Strategy DeMorgan And Generalisation"         $ s
    where
        c = evalCondOnTerm (isMultiAnd . skipNegation)
        s = (c .*. stratRuleTopLayerMany (ruleToStrategy ruleDeMorganAnd) .*. fulltd (try(stratDoubleNotUnary)))
stratDeMorganD        = label "Strategy DeMorgan Derivative"                 $ (stratDeMorgan .*. try (stratDoubleNot))
stratDeMorganOrG      = label "Strategy DeMorgan Or Generalisation"          $ s
    where
        c = evalCondOnTerm (isMultiOr . skipNegation)
        s = (c .*. stratRuleTopLayerMany (ruleToStrategy ruleDeMorganOr) .*. fulltd (try(stratDoubleNotUnary)))
stratDeMorganG        = label "Strategy DeMorgan Generalisation"             $ s
    where
        c = evalCondOnTerm (isMultiAndOr . skipNegation)
        s = (c .*. stratRuleTopLayerMany (ruleToStrategy ruleDeMorgan) .*. fulltd (try(stratDoubleNotUnary)))

ruleDeMorgan         = convertToRule "De Morgan (And / Or)"                  "single.demorgan"                           stratDeMorgan
ruleDeMorganA        = convertToRule "DeMorgan (All variants)"               "all.demorgan"                              stratDeMorganA
ruleDeMorganAndG     = convertToRule "DeMorgan And Generalisation"           "generalisation.demorgan.and"               stratDeMorganAndG
ruleDeMorganD        = convertToRule "DeMorgan and Double Not Derivative"    "combi.deMorgan.doublenot.derivative"       stratDeMorganD
ruleDeMorganOrG      = convertToRule "DeMorgan Or Generalisation"            "generalisation.demorgan.or"                stratDeMorganOrG
ruleDeMorganG        = convertToRule "DeMorgan Generalisation"               "generalisation.demorgan"                   stratDeMorganG

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equivalence Elimination variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratEquivalenceEliminationA = label d                                       $ stratEquivalenceElimination
    where
        d = "Strategy Equivalence Elimination (All Variants)"
stratEquivalenceEliminationD = label d                                       $ s
        where
        f :: SLogic -> Bool
        f (p :<->: q) | hasBool p || hasBool q || skipNegations p == skipNegations q  = True
        f _                                                                           = False 
        d = "Strategy Equivalence Elimination Derivative"
        c = evalCondOnTerm f
        s = (c .*. liftToContext ruleEquivalenceElimination .*. layerAll (try (repeat1 (somewhere (stratUnairies))) .*. 
            try (repeat1 (somewhere (liftToContext ruleIdempotency .|. stratConjunctions)))) .*. 
            try (repeat1 (somewhere (liftToContext ruleIdempotency .|. stratDisjunctions))))
stratEquivalenceElimination = label d                                        $ s
    where
        d = "Strategy Implication Elimination Single + Derivative"
        s = (ruleEquivalenceEliminationD |> liftToContext ruleEquivalenceElimination) 
ruleEquivalenceEliminationD = convertToRule d1                               d2                                          stratEquivalenceEliminationD
    where
        d1 = "Equivalence Elimination Derivative"
        d2 = "combi.equivalenceelimination.derivative"
ruleEquivalenceEliminationA = convertToRule d1                               d2                                          stratEquivalenceEliminationA
    where
        d1 = "Equivalence Elimination  (All Variants)"
        d2 = "all.equivalenceelimination"

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Double Not variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiDoubleNot   = label d s
    where
        d = "Rewrite Strategy Multi Double Not"
        r = liftToContext ruleDoubleNot
        s = evalCondOnTerm isMultiDoubleNot .*. evalCondOnTerm isUnary .*. repeat1 r
stratDoubleNotUnary   = label "Strategy Unary Double Not"                    $ (ruleMultiDoubleNot .|. liftToContext ruleDoubleNot)
stratLayerDoubleNot   = label "Strategy Layered Double Not"                  $ s
    where
        c = evalCondOnTerm (not . isUnary) 
        s = (c .*. repeat1 (layerFirst stratDoubleNotUnary))
stratDoubleNot        = label "Strategy Layered Double Not"                  $ (stratDoubleNotUnary |> stratLayerDoubleNot)

ruleMultiDoubleNot    = convertToRule "Multi Double Not"                     "combi.doublenot"                          stratMultiDoubleNot
ruleLayerDoubleNot    = convertToRule "Layered Double Not"                   "combi.doublenot"                          stratLayerDoubleNot
ruleDoubleNotC        = convertToRule "Double Not (All Variants)"            "all.doublenot"                            stratDoubleNot

-------------------------------------------------------------------------------------------------------------------------------------------------
-- F-Rule Not T/T-Rule Not F variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiTFRuleNotTF = label d                                              $ s
    where
        d = "Strategy Multi Double Not"
        r = (liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT)
        s = (evalCondOnTerm isDoubleNot .*. evalCondOnTerm isUnary .*. repeat1 (oncebu r))
stratTFRuleNotTFUnary = label d                                              $ s
    where
        d = "Strategy Unary TFRuleNotTFUnary"
        s = (ruleMultiTFRuleNotTF |> liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT) 
stratLayerTFRuleNotTF = label "Strategy Layered T-Rule Not F or F-RuleNot-T" $ s
    where
        c = evalCondOnTerm (not . isUnary) 
        s = (c .*. repeat1 (layerFirst stratTFRuleNotTFUnary)) 
stratTFRuleNotTFA     = label "Strategy Layered Double Not"                  $ (stratTFRuleNotTFUnary |> stratLayerTFRuleNotTF) 

ruleMultiTFRuleNotTF  = convertToRule "Multi TRuleNotF / FRuleNotT"          "combi.TRuleNotF.FRuleNotT"                stratMultiTFRuleNotTF
ruleLayerTFRuleNotTF  = convertToRule "Layered TRuleNotF / FRuleNotT"        "combi.layer.TRuleNotF.FRuleNotT"          stratLayerTFRuleNotTF
ruleTFRuleNotTFA      = convertToRule "TRuleNotF / FRuleNotT (All Variants)" "all.TRuleNotF.FRuleNotT"                  stratTFRuleNotTFA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Implication Elimination variants
-------------------------------------------------------------------------------------------------------------------------------------------------
stratImplicationEliminationA = label d                                       $ stratImplicationElimination
    where
        d = "Strategy Implication Elimination (All Variants)"
stratImplicationEliminationD = label d                                       $ s
    where
        f :: SLogic -> Bool
        f (p :->: q) | hasBool p || hasBool q || skipNegations p == skipNegations q  = True
        f _                                                                          = False 
        d = "Strategy Implication Elimination Derivative"
        c = evalCondOnTerm f
        s = (c .*. liftToContext ruleImplicationElimination .*. 
            try (repeat1 (somewhere stratUnairies)) .*. 
            try (repeat1 (somewhere (liftToContext ruleIdempotency .|. stratDisjunctions))))
stratImplicationElimination = label d                                        $ s
    where
        d = "Strategy Implication Elimination Single + Derivative"
        s = ((ruleImplicationEliminationD |> liftToContext ruleImplicationElimination) .*. try (layerLeftMost stratDeMorganA))         
ruleImplicationEliminationA = convertToRule "Implication Elimination (All Variants)" "all.implicationelimination"              stratImplicationEliminationA
ruleImplicationEliminationD = convertToRule "Implication Elimination Derivative"     "combi.implicationelimination.derivative" stratImplicationEliminationD

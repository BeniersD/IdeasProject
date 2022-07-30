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
-- Generic Strategies
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Check function with context 
evalCondOnTerm :: (SLogic -> Bool) -> Strategy (Context SLogic)
evalCondOnTerm c = check (maybe False c . currentInContext)      

-- Executes a strategy
execStrategy :: LabeledStrategy (Context SLogic) -> SLogic -> String
execStrategy s x = show $ applyD s (newContext $ termNavigator x)

-- Strategy checking syntactix similarity
eqExpr :: SLogic -> SLogic -> Bool
eqExpr x y = (execStrategy stratAC x) == (execStrategy stratAC y)

-- Apply layerMany multiple fimes
stratRuleTopLayerMany :: (IsStrategy f, Navigator a, HasId (f a)) => f a -> LabeledStrategy a
stratRuleTopLayerMany f = label d $ s
    where
        d = "Multilayer Many - " ++ showId f
        s = fix $ \x -> f .*. (layerMany x)

stratRuleC, stratNegate :: Rule (Context SLogic) -> LabeledStrategy (Context SLogic)
-- Commutative version of a rule
stratRuleC  r           = label ("Strategy Commutativy of a Rule "++showId r) $ (liftToContext ruleCommutativity .*. r)
stratNegate r           = label ("Strategy Negate " ++ showId r)              $ s 
    where 
        c = evalCondOnTerm isNegation
        s = (c .*. layerFirst r .*. repeatS (somewhere stratUnairiesA))

data StrategyType = Choice | OrElse | Sequence 
multiStrategy :: StrategyType -> [Rule (Context SLogic)] -> LabeledStrategy (Context SLogic)
multiStrategy t xs = label d $ s
    where
        d = "combi" ++ show [i | x <- xs, i <- "." ++ (drop 6 (showId x))]
        s = case t of
                Choice -> choice xs
                OrElse -> orelse xs
                Sequence -> Combinators.sequence xs  
                     
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Visits -- replica of Traversal.hs
-------------------------------------------------------------------------------------------------------------------------------------------------
data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany | VisitLeftMost | VisitRightMost

visit :: (Navigator a, IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit v next s = fix $ \a ->
   case v of
      VisitFirst       -> s  |> next .*. a
      VisitOne         -> s .|. next .*. a
      VisitSome        -> s .*. try (next .*. visit VisitMany next s) .|. next .*. a
      VisitAll         -> s .*. (notS next |> (next .*. a))
      VisitMany        -> try s .*. (notS next |> (next .*. a))

visitm :: (Navigator a, IsStrategy f) => Visit -> f a -> Strategy a
visitm v s = fix $ \a  ->
   case v of
      VisitLeftMost   -> (check (not.hasLeft) .*. s) |> (ruleLeft .*. a)
      VisitRightMost  -> (check (not.hasRight) .*. s) |> (ruleRight .*. a)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- One-layer visits -- replica of Traversal.hs
-------------------------------------------------------------------------------------------------------------------------------------------------
layer :: (Navigator a) => Strategy a -> Strategy a
layer s                = ruleDown .*. s .*. ruleUp

layerAll, layerFirst, layerLeftMost, layerRightMost, layerMany :: (IsStrategy f, Navigator a) => f a -> Strategy a
layerAll s             = layer (visit  VisitAll       ruleRight s)
layerSome s            = layer (visit  VisitSome      ruleRight s)
layerFirst s           = layer (visit  VisitFirst     ruleRight s)
layerLeftMost s        = layer (visitm VisitLeftMost  s)
layerRightMost s       = layer (visitm VisitRightMost s)
layerMany s            = layer (visit  VisitMany      ruleRight s)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Set of logic rewrite strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
ruleFRuleConjunctionC, ruleTRuleConjunctionC, ruleFRuleComplementC, ruleTRuleComplementC, ruleFRuleDisjunctionC, ruleTRuleDisjunctionC, 
    ruleCommutativityOrd, ruleFRuleComplementA, ruleTRuleComplementA, ruleFRuleComplementN, ruleTRuleComplementN, ruleFRuleConjunctionA, 
    ruleFRuleConjunctionN, ruleTRuleConjunctionA, ruleFRuleDisjunctionA, ruleTRuleConjunctionN, ruleTRuleDisjunctionA, ruleFRuleDisjunctionN, 
    ruleTRuleDisjunctionN, ruleDeMorgan, ruleAbsorptionC, ruleAbsorptionA, ruleAbsorptionN, ruleAC, ruleACI, ruleDeMorganAndG, ruleDeMorganOrG, 
    ruleDeMorganG, ruleDeMorganA, ruleDeMorganD, ruleDoubleNotC, ruleMultiDoubleNot, ruleLayerDoubleNot, ruleMultiTFRuleNotTF, 
    ruleLayerTFRuleNotTF, ruleTFRuleNotTFA, ruleImplicationEliminationA, ruleImplicationEliminationN, ruleEquivalenceEliminationA, 
    ruleEquivalenceEliminationN, ruleIdempotencyN, ruleNegations :: Rule (Context SLogic)

stratFRuleComplementC, stratFRuleConjunctionC, stratTRuleConjunctionC, stratTRuleComplementC, stratFRuleDisjunctionC, stratTRuleDisjunctionC, 
    stratCommutativityOrd, stratDeMorgan, stratFRuleComplementA, stratTRuleComplementA, stratFRuleComplementN, stratTRuleComplementN, 
    stratFRuleConjunctionA, stratTRuleConjunctionA, stratFRuleConjunctionN, stratTRuleConjunctionN, stratFRuleDisjunctionA, 
    stratTRuleDisjunctionA, stratFRuleDisjunctionN, stratTRuleDisjunctionN, stratAbsorptionC, stratAbsorptionA, stratAbsorptionN, 
    stratDeMorganAndG, stratDeMorganOrG, stratDeMorganG, stratDeMorganD, stratMultiDoubleNot, stratDoubleNotUnary, stratDoubleNot, 
    stratLayerDoubleNot, stratLayerTFRuleNotTF, stratUnairiesA, stratConjunctionsA, stratMultiTFRuleNotTF, stratTFRuleNotTFUnary, 
    stratTFRuleNotTFA, stratDisjunctionsA, stratAbsorbersA, stratImplicationEliminationD, stratImplicationEliminationA, 
    stratImplicationEliminationN, stratEquivalenceEliminationD, stratImplicationElimination, stratEquivalenceEliminationA, 
    stratEquivalenceElimination, stratEquivalenceEliminationN, stratIdempotencyN, stratNegations, stratSA, stratSAO, stratToNnf, stratToNnfA,
    stratToCnfAC, stratToDnfAC, stratConjunctions, stratDisjunctions, stratAbsorbers, stratGenerics, stratS :: LabeledStrategy (Context SLogic)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Absorption strategies and rules 
-------------------------------------------------------------------------------------------------------------------------------------------------
stratAbsorptionC  = label "Strategy Commutativity-Absortion"                  $ s
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

        la                              = liftToContext ruleAbsorption
        lc                              = liftToContext ruleCommutativity
        hlm                             = (layerLeftMost lc)  .*. la
        hrm                             = (layerRightMost lc) .*. la
        s                               = (evalCondOnTerm f1 .*. hlm )         |>   
                                          (evalCondOnTerm f2 .*. (lc .*. la))  |> 
                                          (evalCondOnTerm f3 .*. hrm )         |>  
                                          (evalCondOnTerm f4 .*. (lc .*. hlm)) |> 
                                          (evalCondOnTerm f5 .*. (lc .*. hrm)) 
stratAbsorptionA = label "Strategy Commutativity-Absortion"                   $ s
    where
        s = ruleAbsorptionD |> ruleAbsorptionC |> liftToContext ruleAbsorption 
stratAbsorptionD = label "Strategy  Absortion Derivative"                     $ s
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
        s1 = evalCondOnTerm f1 .*. ld .*. layerMany (stratTRuleComplementA) .*. stratTRuleConjunctionA
        s2 = evalCondOnTerm f2 .*. ld .*. layerMany (stratFRuleComplementA) .*. stratFRuleDisjunctionA   
        s  = s1 |> s2
stratAbsorptionN = label "Strategy Absortion Negate"                         $ stratNegate ruleAbsorptionA

ruleAbsorptionA  = convertToRule "Commutative Absorption (All variants)"     "all.absorption"                  stratAbsorptionA
ruleAbsorptionC  = convertToRule "Commutative Absorption"                    "combi.absorption.commutativity"  stratAbsorptionC
ruleAbsorptionD  = convertToRule "Commutative Absorption Derivative"         "combi.absorption.derivative"     stratAbsorptionD
ruleAbsorptionN  = convertToRule "Commutative Absorption Derivative"         "combi.absorption.negate"         stratAbsorptionN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Associativity rules 
-------------------------------------------------------------------------------------------------------------------------------------------------
ruleassociativityR :: Rule SLogic
ruleassociativityR    = createRule "Associativity Reversed"                  "single.associativity.reversed"          f
    where
        f ::  SLogic -> Maybe (SLogic)
        f (p :||: (q:||: r))  = Just ((p :||: q) :||: r) 
        f (p :&&: (q :&&: r)) = Just ((p :&&: q) :&&: r)
        f _                   = Nothing

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
        ra = liftToContext ruleAssociativity |> liftToContext rc |> 
             (evalCondOnTerm f .*. lar .*. layerLeftMost (liftToContext ruleCommutativity) .*. la)
        s = repeat1 (somewhere ra)
stratACI                = label d                                            $  s
    where
        d = "Strategy Associativity-Commutativity Idempotency"
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) | p == q = True
        f (p :&&: (q :&&: r)) | p == q = True
        f _                            = False

        lar = liftToContext ruleassociativityR
        ri  = liftToContext ruleIdempotency |> (evalCondOnTerm f .*. lar)
        s   = stratAC .*. repeatS (oncebu ri)

stratUnairiesA          = label d                                            $ stratDoubleNot .|. stratTFRuleNotTFA
    where
        d = "Strategy Double Not / F-Rule Not T / T-Rule Not F (All variants)"
stratConjunctions       = label d                                            $ s
    where
        d = "Strategy F-Rule Complement / F-Rule Conjunction / T-Rule Conjunction (All variants)"
        s = (liftToContext ruleFRuleComplement .|. liftToContext ruleFRuleConjunction .|. liftToContext ruleTRuleConjunction) 
stratConjunctionsA      = label d                                            $ s
    where
        d = "Strategy F-Rule Complement / F-Rule Conjunction / T-Rule Conjunction (All variants)"
        s = (stratFRuleComplementA .|. stratFRuleConjunctionA .|. stratTRuleConjunctionA) 
stratDisjunctions       = label d                                            $ s
    where
        d = "Strategy T-Rule Complement / F-Rule Disjunction / T-Rule Disjunction"
        s = (liftToContext ruleTRuleComplement .|. liftToContext ruleTRuleDisjunction .|. 
            liftToContext ruleFRuleDisjunction) 
stratDisjunctionsA      = label d                                            $ s
    where
        d = "Strategy T-Rule Complement / F-Rule Disjunction / T-Rule Disjunction (All variants)"
        s = (stratTRuleComplementA .|. stratFRuleDisjunctionA .|. stratTRuleDisjunctionA) 
stratAbsorbers          = label d                                            $ s
    where
        d = "Strategy Absorption / Idempotency"
        s = (liftToContext ruleAbsorption .|. liftToContext ruleIdempotency)
stratAbsorbersA         = label d                                            $ s
    where
        d = "Strategy Absorption / Idempotency"
        s = (stratAbsorptionA .|. stratIdempotencyA)
stratGenerics           = label d                                            $ s
    where
        d = "Elimination / Equivalence Elimination / DeMorgan"
        s = (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination .|. stratDeMorgan)
stratGenericsA          = label d                                            $ s
    where
        d = "Elimination / Equivalence Elimination / DeMorgan (All variants)"
        s = (stratImplicationEliminationA .|. stratEquivalenceEliminationA .|. stratDeMorganA)
stratAll                = label d                                            $ s
    where
        d = "Strategy all rules"
        r = (stratNegations |> stratAbsorbersA |> stratConjunctionsA |> stratDisjunctionsA |> stratGenericsA)
        s = repeatS (somewhere stratUnairiesA) .*. try r
stratNegations          = label "Strategy Negate" $ s 
    where 
        c = evalCondOnTerm isNegation
        s = (ruleAbsorptionN .|. ruleIdempotencyN) |> 
            (ruleFRuleComplementN .|. ruleFRuleConjunctionN .|. ruleTRuleConjunctionN) |>
            (ruleTRuleComplementN .|. ruleFRuleDisjunctionN .|. ruleTRuleDisjunctionN) |>
            (ruleImplicationEliminationN .|. ruleEquivalenceEliminationN)
stratS                  = label "Strategy Simplification"                    $ s
    where
        s = repeatS (somewhere (stratDisjunctions .|. stratConjunctions .|. stratAbsorbers)) 
stratSA                 = label "Strategy Simplification"                    $ s
    where
        s = repeatS (somewhere (stratConjunctionsA .|. stratDisjunctionsA .|. stratAbsorbersA)) 
stratSAO                = label "Strategy Simplification and Ordering"       $ s
    where
        s = stratS .*. try stratAC

stratToNnf              = label "Strategy to NNF (All variants)"             $ s
    where
        s = repeatS ( somewhere ( liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination .|. 
            liftToContext ruleDoubleNot .|. stratDeMorgan)) .*. repeatS (somewhere $ liftToContext ruleDoubleNot)
stratToNnfA             = label "Strategy to NNF (AC - All variants)"        $ s
    where
        s = repeatS ( somewhere ( stratImplicationEliminationA .|. stratEquivalenceEliminationA .|. stratNegations .|. 
            stratDeMorganA)) .*. repeatS (somewhere stratUnairiesA)
stratToCnf              = label "Strategy to CNF"                            $ s
    where
        c = evalCondOnTerm isDistAnd  
        s = repeatS ( somewhere (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination)) .*.
            repeatS ( somewhere (liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT)) .*. 
            stratS .*. 
            repeatS ( somewhere (stratDeMorgan)) .*. 
            repeatS ( somewhere $ liftToContext ruleDoubleNot .|. liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT) .*. 
            repeatS ( somewhere $ c .*. liftToContext ruleDistributivity) 
stratToCnfS1            = label "Strategy to CNF (All variants)"             $ s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)
stratToCnfS2            = label "Strategy to CNF (All variants)"             $ s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)  .*. stratSA
stratToCnfAC            = label "Strategy to CNF (AC - All variants)"        $ s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSAO .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)  .*. stratSA

stratToDnf              = label "Strategy to DNF"                            $ s
    where
        c = evalCondOnTerm isDistOr  
        s = repeatS ( somewhere (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination)) .*.
            repeatS ( somewhere (liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT)) .*. 
            stratS .*. 
            repeatS ( somewhere (stratDeMorgan)) .*. 
            repeatS ( somewhere $ liftToContext ruleDoubleNot .|. liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT) .*. 
            repeatS ( somewhere $ c .*. liftToContext ruleDistributivity)

stratToDnfS1            = label "Strategy to DNF (All variants)"             $ s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)
stratToDnfS2            = label "Strategy to DNF (All variants)"             $ s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)  .*. stratSA

stratToDnfAC            = label "Strategy to DNF  (AC - All variants)"       $ s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSAO .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity) .*. stratSA

ruleAC                  = convertToRule "Associativity-Commutativity"        "combi.ac"                                  stratAC  
ruleACI                 = convertToRule d                                    "combi.aci"                                 stratACI    
    where
        d = "Associativity-Commutativity-Idempotency"
ruleNegations           = convertToRule "Negate"                             "combi.negate"                              stratNegations

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Commutativity strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratCommutativityOrd  = label "Strategy Commutativity-Ordered"              $ c .*. liftToContext ruleCommutativity
    where
        c = evalCondOnTerm $ not . isOrdered    

ruleCommutativityOrd   = convertToRule "Commutativity (Ordered variant)"     "single.commutativity.ordered"              stratCommutativityOrd

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Conjunction strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleConjunctionA = label "Strategy F-Rule Conjunction"                 $ s
    where
        s = ruleFRuleConjunctionC |> liftToContext ruleFRuleConjunction  
stratTRuleConjunctionA = label "Strategy T-Rule Conjunction"                 $ s
    where
        s = ruleTRuleConjunctionC |> liftToContext ruleTRuleConjunction
stratFRuleConjunctionC = label "Strategy Commutative-and-F-Rule Conjunction" $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleConjunction) 
    where
        f :: SLogic -> Bool
        f (F :&&: _) = True
        f _          = False  
stratTRuleConjunctionC = label "Strategy Commutative-and-T-Rule Conjunction" $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleConjunction) 
    where
        f :: SLogic -> Bool
        f (T :&&: _) = True
        f _          = False  
stratFRuleConjunctionN = label "Strategy F-Rule Conjunction Negate"         $ stratNegate ruleFRuleConjunctionA
stratTRuleConjunctionN = label "Strategy T-Rule Conjunction Negate"         $ stratNegate ruleTRuleConjunctionA

ruleFRuleConjunctionA  = convertToRule "F-Rule Conjunction (All variants)"  "all.fconjunction"                          stratFRuleConjunctionA
ruleTRuleConjunctionA  = convertToRule "T-Rule Conjunction (All variants)"  "all.tconjunction"                          stratTRuleConjunctionA
ruleFRuleConjunctionC  = convertToRule "Commutative F-Rule Conjunction"     "combi.commutativity.and.fruleconjunction"  stratFRuleConjunctionC
ruleTRuleConjunctionC  = convertToRule "Commutative T-Rule Conjunction"     "combi.commutativity.and.truleconjunction"  stratTRuleConjunctionC
ruleFRuleConjunctionN  = convertToRule "F-Rule Conjunction Negate"          "combi.fconjunction.negate"                 stratFRuleConjunctionN
ruleTRuleConjunctionN  = convertToRule "T-Rule Conjunction Negate"          "combi.tconjunction.negate"                 stratTRuleConjunctionN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Complement strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleComplement   = label "Strategy F-Rule Complement"                  $ ruleFRuleComplementC .|. liftToContext ruleFRuleComplement
stratFRuleComplementA  = label "Strategy F-Rule Complement (All Variants)"   $ ruleFRuleComplementD |> stratFRuleComplement
stratFRuleComplementC  = label "Strategy Commutative-and-F-Rule Complement"  $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleComplement)
    where
        f :: SLogic -> Bool
        f (Not p :&&: q)           | p == q = True
        f _                                 = False  
stratFRuleComplementD  = label "Strategy F-Rule Derivate"                    $ s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :&&: (Not q :&&: r)) | p == q = True
        f1 (Not p :&&: (q :&&: r)) | p == q = True
        f1 _                                = False
        
        f2 (Not p :&&: (q :&&: r)) | p == q = True
        f2 (p :&&: (q :&&: Not r)) | p == r = True
        f2 _                                = False  

        s1 = evalCondOnTerm f1 .*. liftToContext ruleassociativityR .*. layerLeftMost (stratFRuleComplement) 
        s2 = evalCondOnTerm f2 .*. layerRightMost (liftToContext ruleCommutativity) .*. s1
        s  = s1 .|. s2
stratFRuleComplementN  = label "Strategy F-Rule Complement Negate"           $ stratNegate ruleFRuleComplementA
stratTRuleComplement   = label "Strategy T-Rule Complement"                  $ ruleTRuleComplementC .|. liftToContext ruleTRuleComplement
stratTRuleComplementA  = label "Strategy T-Rule Complement (All Variants)"   $ ruleTRuleComplementD |> stratTRuleComplement
stratTRuleComplementC  = label "Strategy Commutative-and-T-Rule Complement"  $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleComplement)
    where
        f :: SLogic -> Bool
        f (Not p :||: q) | p == q           = True
        f _                                 = False 
stratTRuleComplementD  = label "Strategy Commutative-and-T-Rule Complement"  $ s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :||: (Not q :||: r)) | p == q = True
        f1 (Not p :||: (q :||: r)) | p == q = True
        f1 _                                = False
        
        f2 (Not p :||: (q :||: r)) | p == q = True
        f2 (p :||: (q :||: Not r)) | p == r = True
        f2 _                                = False  

        s1 = evalCondOnTerm f1 .*. liftToContext ruleassociativityR .*. layerLeftMost (stratTRuleComplement) 
        s2 = evalCondOnTerm f2 .*. layerRightMost (liftToContext ruleCommutativity) .*. s1
        s  = s1 .|. s2 
stratTRuleComplementN  = label "Strategy T-Rule Complement Negate"           $ stratNegate ruleTRuleComplementA

ruleFRuleComplementA   = convertToRule "F-Rule Complement (All variants)"    "all.fcomplement"                           stratFRuleComplementA
ruleFRuleComplementC   = convertToRule "Commutative F-Rule Complement"       "combi.commutativity.and.frulecomplement"   stratFRuleComplementC
ruleFRuleComplementD   = convertToRule "F-Rule Complement Derivative"        "combi.frulecomplement.derivate"            stratFRuleComplementD
ruleFRuleComplementN   = convertToRule "F-Rule Complement Negate"            "combi.fcomplement.negate"                  stratFRuleComplementN
ruleTRuleComplementA   = convertToRule "T-Rule Complement (All variants)"    "all.tcomplement"                           stratTRuleComplementA
ruleTRuleComplementC   = convertToRule "Commutative T-Rule Complement"       "combi.commutativity.and.trulecomplement"   stratTRuleComplementC
ruleTRuleComplementD   = convertToRule "T-Rule Complement Derivative"        "combi.trulecomplement.derivate"            stratTRuleComplementD
ruleTRuleComplementN   = convertToRule "T-Rule Complement Negate"            "combi.tcomplement.negate"                  stratTRuleComplementN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Disjunction strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratFRuleDisjunctionC = label "Strategy Commutative-and-F-Rule Disjunction" $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleFRuleDisjunction)
    where
        f :: SLogic -> Bool
        f (F :||: _) = True
        f _          = False        
stratTRuleDisjunctionC = label "Strategy Commutative-and-T-Rule Disjunction" $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleDisjunction)
    where
        f :: SLogic -> Bool
        f (T :||: _) = True
        f _          = False        
stratFRuleDisjunctionA = label "Strategy F-Rule Disjunction"                 $ ruleFRuleDisjunctionC |> liftToContext ruleFRuleDisjunction 
stratTRuleDisjunctionA = label "Strategy T-Rule Disjunction"                 $ ruleTRuleDisjunctionC |> liftToContext ruleTRuleDisjunction 
stratFRuleDisjunctionN = label "Strategy F-Rule Disjunction Negate"          $ stratNegate ruleFRuleDisjunctionA
stratTRuleDisjunctionN = label "Strategy T-Rule Disjunction Negate"          $ stratNegate ruleTRuleDisjunctionA

ruleFRuleDisjunctionC  = convertToRule "Commutative F-Rule Disjunction"      "combi.commutativity.and.fruledisjunction"  stratFRuleDisjunctionC
ruleTRuleDisjunctionC  = convertToRule "Commutative T-Rule Disjunction"      "combi.commutativity.and.truledisjunction"  stratTRuleDisjunctionC
ruleFRuleDisjunctionA  = convertToRule "F-Rule Disjunction (All variants)"   "all.fruledisjunction"                      stratFRuleDisjunctionA
ruleTRuleDisjunctionA  = convertToRule "T-Rule Disjunction (All variants)"   "all.truledisjunction"                      stratTRuleDisjunctionA
ruleFRuleDisjunctionN  = convertToRule "F-Rule Disjunction Negate"           "combi.fruledisjunction.negate"             stratFRuleDisjunctionN
ruleTRuleDisjunctionN  = convertToRule "T-Rule Disjunction Negate"           "combi.truledisjunction.negate"             stratTRuleDisjunctionN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- DeMorgan strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratDeMorgan         = label "Strategy DeMorgan"                            $ liftToContext ruleDeMorganOr .|. liftToContext ruleDeMorganAnd
stratDeMorganA        = label "Strategy DeMorgan All Variants)"              $ s
    where
        s = ruleDeMorganAndG |> ruleDeMorganOrG |> ruleDeMorganG |> ruleDeMorganD |> stratDeMorgan
stratDeMorganAndG     = label "Strategy DeMorgan And Generalisation"         $ s
    where
        c = evalCondOnTerm $ isMultiAnd . skipNegation
        s = c .*. stratRuleTopLayerMany (liftToContext ruleDeMorganAnd) .*. fulltd (try stratDoubleNotUnary)
stratDeMorganD        = label "Strategy DeMorgan Derivative"                 $ s 
    where
        f :: SLogic -> Bool
        f (Not p) | hasNegation p = True
        f _                       = False
        c = evalCondOnTerm f
        s = c .*. stratDeMorgan .*. stratDoubleNot
stratDeMorganOrG      = label "Strategy DeMorgan Or Generalisation"          $ s
    where
        c = evalCondOnTerm $ isMultiOr . skipNegation
        s = c .*. stratRuleTopLayerMany (liftToContext ruleDeMorganOr) .*. fulltd (try stratDoubleNotUnary)
stratDeMorganG        = label "Strategy DeMorgan Generalisation"             $ s
    where
        c = evalCondOnTerm $ isMultiAndOr . skipNegation
        s = c .*. stratRuleTopLayerMany ruleDeMorgan .*. fulltd (try stratDoubleNotUnary)

ruleDeMorgan         = convertToRule "De Morgan (And / Or)"                  "single.demorgan"                           stratDeMorgan
ruleDeMorganA        = convertToRule "DeMorgan (All variants)"               "all.demorgan"                              stratDeMorganA
ruleDeMorganAndG     = convertToRule "DeMorgan And Generalisation"           "generalisation.demorgan.and"               stratDeMorganAndG
ruleDeMorganD        = convertToRule "DeMorgan and Double Not Derivative"    "combi.deMorgan.doublenot.derivative"       stratDeMorganD
ruleDeMorganOrG      = convertToRule "DeMorgan Or Generalisation"            "generalisation.demorgan.or"                stratDeMorganOrG
ruleDeMorganG        = convertToRule "DeMorgan Generalisation"               "generalisation.demorgan"                   stratDeMorganG

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equivalence Elimination strategies and rules
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
        s = (c .*. liftToContext ruleEquivalenceElimination .*. layerAll (repeatS (somewhere (stratUnairiesA)) .*. 
            repeatS (somewhere $ liftToContext ruleIdempotency .|. stratConjunctionsA)) .*. 
            repeatS (somewhere $ liftToContext ruleIdempotency .|. stratDisjunctionsA))
stratEquivalenceElimination  = label d                                       $ s
    where
        d = "Strategy Implication Elimination Single + Derivative"
        s = ruleEquivalenceEliminationD |> liftToContext ruleEquivalenceElimination
stratEquivalenceEliminationN = label d                                       $ stratNegate ruleEquivalenceEliminationA
    where
        d = "Strategy Equivalence Elimination Negate"

ruleEquivalenceEliminationD  = convertToRule d1                               d2                                          stratEquivalenceEliminationD
    where
        d1 = "Equivalence Elimination Derivative"
        d2 = "combi.equivalenceelimination.derivative"
ruleEquivalenceEliminationA  = convertToRule d1                               "all.equivalenceelimination"                stratEquivalenceEliminationA
    where
        d1 = "Equivalence Elimination  (All Variants)"
ruleEquivalenceEliminationN  = convertToRule "Equivalence Elimination Negate" "combi.equivalenceelimination.negate"       stratEquivalenceEliminationN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Double Not strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiDoubleNot   = label d                                              $ s
    where
        d = "Rewrite Strategy Multi Double Not"
        c = evalCondOnTerm isMultiDoubleNot .*. evalCondOnTerm isUnary
        s = c .*. repeat1 (liftToContext ruleDoubleNot)
stratDoubleNotUnary   = label "Strategy Unary Double Not"                    $ (ruleMultiDoubleNot .|. liftToContext ruleDoubleNot)
stratLayerDoubleNot   = label "Strategy Layered Double Not"                  $ s
    where
        c = evalCondOnTerm $ not . isUnary
        s = c .*. repeat1 (layerFirst stratDoubleNotUnary)
stratDoubleNot        = label "Strategy Layered Double Not"                  $ (stratDoubleNotUnary |> stratLayerDoubleNot)

ruleMultiDoubleNot    = convertToRule "Multi Double Not"                     "combi.doublenot"                          stratMultiDoubleNot
ruleLayerDoubleNot    = convertToRule "Layered Double Not"                   "combi.doublenot"                          stratLayerDoubleNot
ruleDoubleNotC        = convertToRule "Double Not (All Variants)"            "all.doublenot"                            stratDoubleNot

-------------------------------------------------------------------------------------------------------------------------------------------------
-- F-Rule Not T/T-Rule Not F strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiTFRuleNotTF = label d                                              $ s
    where
        d = "Strategy Multi Double Not"
        r = liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT
        s = evalCondOnTerm isDoubleNot .*. evalCondOnTerm isUnary .*. repeat1 (oncebu r)
stratTFRuleNotTFUnary = label d                                              $ s
    where
        d = "Strategy Unary TFRuleNotTFUnary"
        s = ruleMultiTFRuleNotTF |> liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT 
stratLayerTFRuleNotTF = label "Strategy Layered T-Rule Not F or F-RuleNot-T" $ s
    where
        c = evalCondOnTerm $ not . isUnary 
        s = c .*. repeat1 (layerFirst stratTFRuleNotTFUnary) 
stratTFRuleNotTFA     = label "Strategy Layered Double Not"                  $ (stratTFRuleNotTFUnary |> stratLayerTFRuleNotTF) 

ruleMultiTFRuleNotTF  = convertToRule "Multi TRuleNotF / FRuleNotT"          "combi.TRuleNotF.FRuleNotT"                stratMultiTFRuleNotTF
ruleLayerTFRuleNotTF  = convertToRule "Layered TRuleNotF / FRuleNotT"        "combi.layer.TRuleNotF.FRuleNotT"          stratLayerTFRuleNotTF
ruleTFRuleNotTFA      = convertToRule "TRuleNotF / FRuleNotT (All Variants)" "all.TRuleNotF.FRuleNotT"                  stratTFRuleNotTFA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Idempotency strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratIdempotencyA     = label "Strategy Idempotency Negate"                  $ ruleIdempotencyD |> liftToContext ruleIdempotency
stratIdempotencyD     = label "Strategy Idempotency Negate"                  $ s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :&&: (q :&&: r)) | p == q     = True
        f1 (p :||: (q :||: r)) | p == q     = True
        f1 _                                = False

        f2 (p :&&: (q :&&: r)) | p == r     = True
        f2 (p :||: (q :||: r)) | p == r     = True
        f2 _                                = False

        s1 = evalCondOnTerm f1 .*. liftToContext ruleassociativityR .*. layerLeftMost (liftToContext ruleIdempotency) 
        s2 = evalCondOnTerm f2 .*. layerRightMost (liftToContext ruleCommutativity) .*. s1
        s  = s1 .|. s2
stratIdempotencyN     = label "Strategy Idempotency Negate"                  $ stratNegate ruleIdempotencyA

ruleIdempotencyA      = convertToRule "Idempotency Negate"                   "all.idempotency"                           stratIdempotencyA
ruleIdempotencyD      = convertToRule "Idempotency Negate"                   "combi.idempotency.derivate"                stratIdempotencyD
ruleIdempotencyN      = convertToRule "Idempotency Negate"                   "combi.idempotency.negate"                  stratIdempotencyN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Implication Elimination strategies and rules
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
        s = c .*. liftToContext ruleImplicationElimination .*. 
            repeatS (somewhere stratUnairiesA) .*. 
            repeatS (somewhere (liftToContext ruleIdempotency .|. stratDisjunctionsA))
stratImplicationElimination = label d                                        $ s
    where
        d = "Strategy Implication Elimination Single + Derivative"
        s = (ruleImplicationEliminationD |> liftToContext ruleImplicationElimination) .*. try (layerLeftMost stratDeMorganA)        
stratImplicationEliminationN = label d                                       $ stratNegate ruleImplicationEliminationA
    where
        d = "Strategy Implication Elimination Negate"

ruleImplicationEliminationA = convertToRule "Implication Elimination (All Variants)" "all.implicationelimination"              stratImplicationEliminationA
ruleImplicationEliminationD = convertToRule "Implication Elimination Derivative"     "combi.implicationelimination.derivative" stratImplicationEliminationD
ruleImplicationEliminationN = convertToRule "Implication Elimination Negate"         "combi.equivalenceelimination.negate"     stratImplicationEliminationN
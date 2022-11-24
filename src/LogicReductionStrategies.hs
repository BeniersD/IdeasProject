{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicReductionStrategies where

import Ideas.Common.Strategy hiding (not, layer)
import Data.List
import Data.Maybe
import Domain.Logic.Formula 
import qualified Ideas.Common.Strategy.Combinators as Combinators
import Ideas.Common.Library hiding (layer)
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
eqExpr x y = (execStrategy stratToAC x) == (execStrategy stratToAC y)

-- Apply layerMany multiple fimes
stratRuleTopLayerMany :: (IsStrategy f, Navigator a, HasId (f a)) => f a -> LabeledStrategy a
stratRuleTopLayerMany f = label d $ s
    where
        d = "Multilayer Many - " ++ showId f
        s = fix $ \x -> f .*. (layerMany x)

stratRuleC, stratNegate :: Rule (Context SLogic) -> LabeledStrategy (Context SLogic)
-- Commutative version of a rule
stratRuleC  r           = label ("Strategy Commutativy of a Rule " ++ showId r) $ (liftToContext ruleCommutativity .*. r)
stratNegate r           = label ("Strategy Negate " ++ showId r) s 
    where 
        c = evalCondOnTerm isNegation
        s = (c .*. layerFirst r .*. repeatS (somewhere stratBoolsAndDoubleNotA))

data StrategyType = Choice | OrElse | Sequence 
multiStrategy :: StrategyType -> [Rule (Context SLogic)] -> LabeledStrategy (Context SLogic)
multiStrategy t xs = label d $ s
    where
        d = "combi" ++ show [i | x <- xs, i <- "." ++ (drop 6 (showId x))]
        s = case t of
                Choice   -> choice xs
                OrElse   -> orelse xs
                Sequence -> Combinators.sequence xs  
 
evalStrategyG :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
evalStrategyG l s       = label l $ s

getAllRules :: [Rule (Context SLogic)]
getAllRules = removeDuplicates $ rulesInStrategy $ stratNegations .|. stratAll

applicableRules :: SLogic -> [Rule (Context SLogic)]
applicableRules l = zs
    where
        f :: [Rule (Context SLogic)] -> SLogic -> [Rule (Context SLogic)]
        f [] lf  = []
        f (x:xs) lf | (hasRule x lf) = [x] ++ f xs lf
                    | otherwise      = []  ++ f xs lf
        ys = f getAllRules l
        zs = case (elem (liftToContext ruleDeMorganOr) ys || elem (liftToContext ruleDeMorganAnd) ys) of
                  True  -> [ruleDeMorgan] ++ filter (\x -> showId x /= "single.demorgan.or" && showId x /= "single.demorgan.and" ) ys
                  False -> ys

evalApplicableRules :: SLogic -> [Rule (Context SLogic)] -> [SLogic]
evalApplicableRules l xs = [fromJust $ currentInContext (applyD (repeat1 $ somewhere x) (newContext $ termNavigator l)) | x <- xs]

evalApplicableRulesAC :: SLogic -> [Rule (Context SLogic)] -> [SLogic]
evalApplicableRulesAC l xs = [fromJust $ currentInContext (applyD stratToAC (newContext $ termNavigator x)) | x <- (evalApplicableRules l xs)]

data MultiSingleStrategy = AcEnabled | AcDisabled
isMultiSingleRule :: SLogic -> SLogic -> MultiSingleStrategy -> Bool
isMultiSingleRule l1 l2 b =  matchingElement ys zs
    where
        xs = applicableRules l1
        f = case b of
                AcEnabled  -> evalApplicableRulesAC
                AcDisabled -> evalApplicableRules
        ys = f l1 xs
        zs = f l2 xs


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
    ruleTRuleDisjunctionN, ruleDeMorgan, ruleAbsorptionC, ruleAbsorptionA, ruleAbsorptionN, ruleToAC, ruleToACI, ruleDeMorganAndG, ruleDeMorganOrG, 
    ruleDeMorganG, ruleDeMorganA, ruleDeMorganD, ruleDoubleNotA, ruleMultiDoubleNot, ruleLayerDoubleNot, ruleMultiTFRuleNotTF, 
    ruleLayerTFRuleNotTF, ruleTFRuleNotTFA, ruleImplicationEliminationA, ruleImplicationEliminationN, ruleEquivalenceEliminationA, 
    ruleEquivalenceEliminationN, ruleIdempotencyN, ruleNegations, ruleImplicationEliminationD :: Rule (Context SLogic)

stratFRuleComplementC, stratFRuleConjunctionC, stratTRuleConjunctionC, stratTRuleComplementC, stratFRuleDisjunctionC, stratTRuleDisjunctionC, 
    stratCommutativityOrd, stratDeMorgan, stratFRuleComplementA, stratTRuleComplementA, stratFRuleComplementN, stratTRuleComplementN, 
    stratFRuleConjunctionA, stratTRuleConjunctionA, stratFRuleConjunctionN, stratTRuleConjunctionN, stratFRuleDisjunctionA, 
    stratTRuleDisjunctionA, stratFRuleDisjunctionN, stratTRuleDisjunctionN, stratAbsorptionC, stratAbsorptionA, stratAbsorptionN, 
    stratDeMorganAndG, stratDeMorganOrG, stratDeMorganG, stratDeMorganD, stratMultiDoubleNot, stratDoubleNot, stratDoubleNotA, 
    stratLayerDoubleNot, stratLayerTFRuleNotTF, stratBoolsAndDoubleNotA, stratConjunctionsA, stratMultiTFRuleNotTF, stratTFRuleNotTFBool, 
    stratTFRuleNotTFA, stratDisjunctionsA, stratAbsorbersA, stratImplicationEliminationD, stratImplicationEliminationA, 
    stratImplicationEliminationN, stratEquivalenceEliminationD, stratImplicationElimination, stratEquivalenceEliminationA, 
    stratEquivalenceElimination, stratEquivalenceEliminationN, stratIdempotencyN, stratNegations, stratSA, stratSAO, stratToNnf, stratToNnfA,
    stratToCnfAC, stratToDnfAC, stratConjunctions, stratDisjunctions, stratAbsorbers, stratGenerics, stratS :: LabeledStrategy (Context SLogic)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Absorption strategies and rules 
-------------------------------------------------------------------------------------------------------------------------------------------------
stratAbsorptionC  = label "Strategy Commutativity-Absortion" s
    where
        f1, f2, f3, f4, f5 :: SLogic -> Bool 
        f1 ((p :&&: q) :||: r) = p == r 
        f1 _                   = False

        f2 (p :||: (q :&&: r)) = p == r  
        f2 ((p :||: q) :&&: r) = p == r 
        f2 _                   = False

        f3 (p :&&: (q :||: r)) = p == r 
        f3 _                   = False

        f4 (p :||: (q :&&: r)) | p == q = True 
        f4 _                            = False

        f5 ((p :||: q) :&&: r) = q == r 
        f5 _                   = False

        la                     = liftToContext ruleAbsorption
        lc                     = liftToContext ruleCommutativity
        hlm                    = (layerLeftMost lc)  .*. la
        hrm                    = (layerRightMost lc) .*. la
        s                      = (evalCondOnTerm f1 .*. hlm )         |>   
                                 (evalCondOnTerm f2 .*. (lc .*. la))  |> 
                                 (evalCondOnTerm f3 .*. hrm )         |>  
                                 (evalCondOnTerm f4 .*. (lc .*. hlm)) |> 
                                 (evalCondOnTerm f5 .*. (lc .*. hrm)) 
stratAbsorptionA = label "Strategy Commutativity-Absortion" s
    where
        s = ruleAbsorptionD |> ruleAbsorptionC |> liftToContext ruleAbsorption 
stratAbsorptionD = label "Strategy  Absortion Derivative" s
    where    
        f1, f2 :: SLogic -> Bool
        f1 ((Not p :&&: q) :||: r) = p == r           
        f1 ((p :&&: Not q) :||: r) = q == r           
        f1 ((p :&&: q) :||: Not r) = p == r || q == r 
        f1 (Not p :||: (q :&&: r)) = p == q || p == r 
        f1 (p :||: (Not q :&&: r)) = p == q           
        f1 (p :||: (q :&&: Not r)) = p == r 
        f1 _                       = False          


        f2 ((Not p :||: q) :&&: r) = p == r           
        f2 ((p :||: Not q) :&&: r) = q == r           
        f2 ((p :||: q) :&&: Not r) = p == r || q == r  
        f2 (Not p :&&: (q :||: r)) = p == q || p == r 
        f2 (p :&&: (Not q :||: r)) = p == q           
        f2 (p :&&: (q :||: Not r)) = p == r           
        f2 _                       = False   

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
ruleAssociativityR :: Rule SLogic
ruleAssociativityR    = createRule "Associativity Reversed"                  "single.associativity.reversed"          f
    where
        f ::  SLogic -> Maybe (SLogic)
        f (p :||: (q:||: r))  = Just ((p :||: q) :||: r) 
        f (p :&&: (q :&&: r)) = Just ((p :&&: q) :&&: r)
        f _                   = Nothing

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Combinated strategies like
-- - Combination of associativity/ commutativity
-- - Combination of associativity/ commutativity and Idempotency  
-- - Combination of bools and literals
-- - Combination of conjunctions
-- - Combination of disjunctions
-------------------------------------------------------------------------------------------------------------------------------------------------
stratToAC, stratToACI :: LabeledStrategy (Context SLogic)
stratToAC               = label "Strategy Associativity-Commutativity" s
    where
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :||: q)                             = True                            
        f (p :||: (q :||: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :||: skipNegations q) = True 
        f (p :&&: (q :&&: r)) | (skipNegations p == skipNegations q) && (not . isOrdered) (p :&&: q)                             = True                            
        f (p :&&: (q :&&: r)) | (skipNegations p /= skipNegations q) && (not . isOrdered) (skipNegations p :&&: skipNegations q) = True 
        f _                                                                                                                      = False
        lar = liftToContext ruleAssociativityR
        la  = liftToContext ruleAssociativity
        rc = (check (not . isAssoCommOrdered) .*. ruleCommutativity) 
        ra = liftToContext ruleAssociativity |> liftToContext rc |> 
             (evalCondOnTerm f .*. lar .*. layerLeftMost (liftToContext ruleCommutativity) .*. la)
        s = repeat1 (somewhere ra)
stratToACI              = label "Strategy Associativity-Commutativity Idempotency" s
    where
        f ::  SLogic -> Bool
        f (p :||: (q :||: r)) = p == q 
        f (p :&&: (q :&&: r)) = p == q 
        f _                   = False

        lar = liftToContext ruleAssociativityR
        ri  = liftToContext ruleIdempotency |> (evalCondOnTerm f .*. lar)
        s   = repeatS (somewhere stratBoolsAndDoubleNotA) .*. stratToAC .*. repeatS (oncebu ri)

stratBoolsAndDoubleNotA  = label "Strategy Double Not / F-Rule Not T / T-Rule Not F (All variants)" $ stratDoubleNotA .|. stratTFRuleNotTFA
stratConjunctions       = label "Strategy F-Rule Complement / F-Rule Conjunction / T-Rule Conjunction (All variants)"  s
    where
        s = (liftToContext ruleFRuleComplement .|. liftToContext ruleFRuleConjunction .|. liftToContext ruleTRuleConjunction) 
stratConjunctionsA      = label "Strategy F-Rule Complement / F-Rule Conjunction / T-Rule Conjunction (All variants)"  s
    where
        s = (stratFRuleComplementA .|. stratFRuleConjunctionA .|. stratTRuleConjunctionA) 
stratDisjunctions       = label "Strategy T-Rule Complement / F-Rule Disjunction / T-Rule Disjunction"                 s
    where
        s = (liftToContext ruleTRuleComplement .|. liftToContext ruleTRuleDisjunction .|. 
            liftToContext ruleFRuleDisjunction) 
stratDisjunctionsA      = label "Strategy T-Rule Complement / F-Rule Disjunction / T-Rule Disjunction (All variants)"  s
    where
        s = (stratTRuleComplementA .|. stratFRuleDisjunctionA .|. stratTRuleDisjunctionA) 
stratAbsorbers          = label "Strategy Absorption / Idempotency"                                                    s
    where
        s = (liftToContext ruleAbsorption .|. liftToContext ruleIdempotency)
stratAbsorbersA         = label "Strategy Absorption / Idempotency" $ (stratAbsorptionA .|. stratIdempotencyA)
stratGenerics           = label "Strategy Implication Elimination / Equivalence Elimination / DeMorgan"                s
    where
        s = (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination .|. stratDeMorgan)
stratGenericsA          = label "Strategy Implication Elimination / Equivalence Elimination / DeMorgan (All variants)" s
    where
        s = (stratImplicationEliminationA .|. stratEquivalenceEliminationA .|. stratDeMorganA)
stratAll                = label "Strategy all rules"                                                                   s
    where
        r = (stratNegations |> stratAbsorbersA |> stratConjunctionsA |> stratDisjunctionsA |> stratGenericsA)
        s = repeatS (somewhere stratBoolsAndDoubleNotA) .*. try r
stratNegations          = label "Strategy Negate" $ s 
    where 
        c = evalCondOnTerm isNegation
        s = (ruleAbsorptionN .|. ruleIdempotencyN) |> 
            (ruleFRuleComplementN .|. ruleFRuleConjunctionN .|. ruleTRuleConjunctionN) |>
            (ruleTRuleComplementN .|. ruleFRuleDisjunctionN .|. ruleTRuleDisjunctionN) |>
            (ruleImplicationEliminationN .|. ruleEquivalenceEliminationN)
stratS                  = label "Strategy Simplification"              s
    where
        s = repeatS (somewhere (stratDisjunctions .|. stratConjunctions .|. stratAbsorbers)) 
stratSA                 = label "Strategy Simplification"              s
    where
        s = repeatS (somewhere (stratConjunctionsA .|. stratDisjunctionsA .|. stratAbsorbersA)) 
stratSAO                = label "Strategy Simplification and Ordering" s
    where
        s = repeatS( repeat1 (somewhere (stratConjunctionsA .|. stratDisjunctionsA .|. stratAbsorbersA)) |> stratToAC )

stratToNnf              = label "Strategy to NNF (All variants)"       s
    where
        s = repeatS ( somewhere ( liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination .|. 
            liftToContext ruleDoubleNot .|. stratDeMorgan)) .*. repeatS (somewhere $ liftToContext ruleDoubleNot)
stratToNnfA             = label "Strategy to NNF (AC - All variants)"  s
    where
        s = repeatS ( somewhere ( stratImplicationEliminationA .|. stratEquivalenceEliminationA .|. stratNegations .|. 
            stratDeMorganA)) .*. repeatS (somewhere stratBoolsAndDoubleNotA)
stratToCnf              = label "Strategy to CNF"                      s
    where
        c = evalCondOnTerm isDistAnd  
        s = repeatS ( somewhere (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination)) .*.
            repeatS ( somewhere (liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT)) .*. 
            stratS .*. 
            repeatS ( somewhere (stratDeMorgan)) .*. 
            repeatS ( somewhere $ liftToContext ruleDoubleNot .|. liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT) .*. 
            repeatS ( somewhere $ c .*. liftToContext ruleDistributivity) 
stratToCnfS1            = label "Strategy to CNF (All variants)"       s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)
stratToCnfS2            = label "Strategy to CNF (All variants)"       s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity) .*. stratSA

stratToCnfAC            = label "Strategy to CNF (AC - All variants)"  s
    where
        c = evalCondOnTerm isDistAnd  
        s = stratToNnfA .*. stratSAO .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity) .*. stratSAO  
stratToDnf              = label "Strategy to DNF"                      s
    where
        c = evalCondOnTerm isDistOr  
        s = repeatS ( somewhere (liftToContext ruleImplicationElimination .|. liftToContext ruleEquivalenceElimination)) .*.
            repeatS ( somewhere (liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT)) .*. 
            stratS .*. 
            repeatS ( somewhere (stratDeMorgan)) .*. 
            repeatS ( somewhere $ liftToContext ruleDoubleNot .|. liftToContext ruleTRuleNotF .|. liftToContext ruleFRuleNotT) .*. 
            repeatS ( somewhere $ c .*. liftToContext ruleDistributivity)

stratToDnfS1            = label "Strategy to DNF (All variants)"       s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)
stratToDnfS2            = label "Strategy to DNF (All variants)"       s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSA .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity)  .*. stratSA


stratToDnfAC            = label "Strategy to DNF  (AC - All variants)" s
    where
        c = evalCondOnTerm isDistOr  
        s = stratToNnfA .*. stratSAO .*. repeatS (somewhere $ c .*. liftToContext ruleDistributivity) .*. stratSAO 
ruleToAC                = convertToRule "Associativity-Commutativity"             "combi.ac"     stratToAC  
ruleToACI               = convertToRule "Associativity-Commutativity-Idempotency" "combi.aci"    stratToACI    
ruleNegations           = convertToRule "Negate"                                  "combi.negate" stratNegations

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
stratFRuleConjunctionA = label "Strategy F-Rule Conjunction"                 s
    where
        s = ruleFRuleConjunctionC |> liftToContext ruleFRuleConjunction  
stratTRuleConjunctionA = label "Strategy T-Rule Conjunction"                 s
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
        f (Not p :&&: q)           = p == q 
        f _                        = False  
stratFRuleComplementD  = label "Strategy F-Rule Derivate"                    s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :&&: (Not q :&&: r)) = p == q 
        f1 (Not p :&&: (q :&&: r)) = p == q 
        f1 _                       = False
        
        f2 (Not p :&&: (q :&&: r)) = p == q
        f2 (p :&&: (q :&&: Not r)) = p == r
        f2 _                       = False  

        s1 = evalCondOnTerm f1 .*. liftToContext ruleAssociativityR .*. layerLeftMost (stratFRuleComplement) 
        s2 = evalCondOnTerm f2 .*. layerRightMost (liftToContext ruleCommutativity) .*. s1
        s  = s1 .|. s2
stratFRuleComplementN  = label "Strategy F-Rule Complement Negate"           $ stratNegate ruleFRuleComplementA
stratTRuleComplement   = label "Strategy T-Rule Complement"                  $ ruleTRuleComplementC .|. liftToContext ruleTRuleComplement
stratTRuleComplementA  = label "Strategy T-Rule Complement (All Variants)"   $ ruleTRuleComplementD |> stratTRuleComplement
stratTRuleComplementC  = label "Strategy Commutative-and-T-Rule Complement"  $ evalCondOnTerm f .*. stratRuleC (liftToContext ruleTRuleComplement)
    where
        f :: SLogic -> Bool
        f (Not p :||: q) = p == q
        f _              = False 
stratTRuleComplementD  = label "Strategy Commutative-and-T-Rule Complement"  s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :||: (Not q :||: r)) = p == q 
        f1 (Not p :||: (q :||: r)) = p == q 
        f1 _                       = False
        
        f2 (Not p :||: (q :||: r)) = p == q 
        f2 (p :||: (q :||: Not r)) = p == r 
        f2 _                       = False  

        s1 = evalCondOnTerm f1 .*. liftToContext ruleAssociativityR .*. layerLeftMost (stratTRuleComplement) 
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
stratDeMorganA        = label "Strategy DeMorgan All Variants)"              s
    where
        s = ruleDeMorganAndG |> ruleDeMorganOrG |> ruleDeMorganG |> ruleDeMorganD |> stratDeMorgan
stratDeMorganAndG     = label "Strategy DeMorgan And Generalisation"         s
    where
        c = evalCondOnTerm $ isMultiAnd . skipNegation
        s = c .*. stratRuleTopLayerMany (liftToContext ruleDeMorganAnd) .*. fulltd (try stratDoubleNotA)
stratDeMorganD        = label "Strategy DeMorgan Derivative"                 s 
    where
        f :: SLogic -> Bool
        f (Not p) | hasNegation p = True
        f _                       = False
        c = evalCondOnTerm f
        s = c .*. stratDeMorgan .*. fulltd (try stratDoubleNotA)
stratDeMorganOrG      = label "Strategy DeMorgan Or Generalisation"          s
    where
        c = evalCondOnTerm $ isMultiOr . skipNegation
        s = c .*. stratRuleTopLayerMany (liftToContext ruleDeMorganOr) .*. fulltd (try stratDoubleNotA)
stratDeMorganG        = label "Strategy DeMorgan Generalisation"             s
    where
        c = evalCondOnTerm $ isMultiAndOr . skipNegation
        s = c .*. stratRuleTopLayerMany ruleDeMorgan .*. fulltd (try stratDoubleNot)

ruleDeMorgan         = convertToRule "De Morgan (And / Or)"                  "single.demorgan"                           stratDeMorgan
ruleDeMorganA        = convertToRule "DeMorgan (All variants)"               "all.demorgan"                              stratDeMorganA
ruleDeMorganAndG     = convertToRule "DeMorgan And Generalisation"           "generalisation.demorgan.and"               stratDeMorganAndG
ruleDeMorganD        = convertToRule "DeMorgan and Double Not Derivative"    "combi.deMorgan.doublenot.derivative"       stratDeMorganD
ruleDeMorganOrG      = convertToRule "DeMorgan Or Generalisation"            "generalisation.demorgan.or"                stratDeMorganOrG
ruleDeMorganG        = convertToRule "DeMorgan Generalisation"               "generalisation.demorgan"                   stratDeMorganG

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equivalence Elimination strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratEquivalenceEliminationA = label "Strategy Equivalence Elimination (All Variants)"      stratEquivalenceElimination

stratEquivalenceEliminationD = label "Strategy Equivalence Elimination Derivative"          s
        where
        f :: SLogic -> Bool
        f (p :<->: q) | hasBool p || hasBool q || skipNegations p == skipNegations q  = True
        f _                                                                           = False 
        c = evalCondOnTerm f
        s = (c .*. liftToContext ruleEquivalenceElimination .*. layerAll (repeatS (somewhere (stratBoolsAndDoubleNotA)) .*. 
            repeatS (somewhere $ liftToContext ruleIdempotency .|. stratConjunctionsA)) .*. 
            repeatS (somewhere $ liftToContext ruleIdempotency .|. stratDisjunctionsA))
stratEquivalenceElimination  = label "Strategy Implication Elimination Single + Derivative" s
    where
        s = ruleEquivalenceEliminationD |> liftToContext ruleEquivalenceElimination
stratEquivalenceEliminationN = label "Strategy Equivalence Elimination Negate"              $ stratNegate ruleEquivalenceEliminationA

ruleEquivalenceEliminationD  = convertToRule "Equivalence Elimination Derivative" "combi.equivalenceelimination.derivative" stratEquivalenceEliminationD
ruleEquivalenceEliminationA  = convertToRule "Equivalence Elimination  (All Variants)" "all.equivalenceelimination" stratEquivalenceEliminationA
ruleEquivalenceEliminationN  = convertToRule "Equivalence Elimination Negate" "combi.equivalenceelimination.negate"       stratEquivalenceEliminationN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Double Not strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiDoubleNot   = label "Rewrite Strategy Multi Double Not" s
    where
        c = evalCondOnTerm isMultiDoubleNot
        s = c .*. repeat1 (liftToContext ruleDoubleNot)
stratDoubleNot = label "Strategy Double Not"       $ (ruleMultiDoubleNot .|. liftToContext ruleDoubleNot)
stratLayerDoubleNot   = label "Strategy Layered Double Not"       s
    where
        c = evalCondOnTerm $ not . isLiteral
        s = c .*. repeat1 (layerFirst stratDoubleNot)
stratDoubleNotA        = label "Strategy Layered Double Not"       $ stratDoubleNot |> stratLayerDoubleNot

ruleMultiDoubleNot    = convertToRule "Multi Double Not"          "combi.doublenot" stratMultiDoubleNot
ruleLayerDoubleNot    = convertToRule "Layered Double Not"        "combi.doublenot" stratLayerDoubleNot
ruleDoubleNotA        = convertToRule "Double Not (All Variants)" "all.doublenot"   stratDoubleNotA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- F-Rule Not T/T-Rule Not F strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratMultiTFRuleNotTF = label d                                              s
    where
        d = "Strategy Multi Double Not"
        r = liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT
        s = evalCondOnTerm isDoubleNot .*. evalCondOnTerm isLiteral .*. repeat1 (oncebu r)
stratTFRuleNotTFBool = label d                                               s
    where
        d = "Strategy T-Rule Not F / F-Rule Not T"
        s = ruleMultiTFRuleNotTF |> liftToContext ruleTRuleNotF |> liftToContext ruleFRuleNotT 
stratLayerTFRuleNotTF = label "Strategy Layered T-Rule Not F / F-Rule Not T" s
    where
        c = evalCondOnTerm $ not . isLiteral 
        s = c .*. repeat1 (layerFirst stratTFRuleNotTFBool) 
stratTFRuleNotTFA     = label "Strategy Layered Double Not"                  $ stratTFRuleNotTFBool |> stratLayerTFRuleNotTF 

ruleMultiTFRuleNotTF  = convertToRule "Multi TRuleNotF / FRuleNotT"          "combi.TRuleNotF.FRuleNotT"                stratMultiTFRuleNotTF
ruleLayerTFRuleNotTF  = convertToRule "Layered TRuleNotF / FRuleNotT"        "combi.layer.TRuleNotF.FRuleNotT"          stratLayerTFRuleNotTF
ruleTFRuleNotTFA      = convertToRule "TRuleNotF / FRuleNotT (All Variants)" "all.TRuleNotF.FRuleNotT"                  stratTFRuleNotTFA

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Idempotency strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratIdempotencyA     = label "Strategy Idempotency Negate"                  $ ruleIdempotencyD |> liftToContext ruleIdempotency
stratIdempotencyD     = label "Strategy Idempotency Negate"                  s
    where
        f1, f2 :: SLogic -> Bool
        f1 (p :&&: (q :&&: r)) = p == q
        f1 (p :||: (q :||: r)) = p == q
        f1 _                   = False

        f2 (p :&&: (q :&&: r)) = p == r
        f2 (p :||: (q :||: r)) = p == r
        f2 _                   = False

        s1 = evalCondOnTerm f1 .*. liftToContext ruleAssociativityR .*. layerLeftMost (liftToContext ruleIdempotency) 
        s2 = evalCondOnTerm f2 .*. layerRightMost (liftToContext ruleCommutativity) .*. s1
        s  = s1 .|. s2
stratIdempotencyN     = label "Strategy Idempotency Negate"                  $ stratNegate ruleIdempotencyA

ruleIdempotencyA      = convertToRule "Idempotency Negate"                   "all.idempotency"                           stratIdempotencyA
ruleIdempotencyD      = convertToRule "Idempotency Negate"                   "combi.idempotency.derivate"                stratIdempotencyD
ruleIdempotencyN      = convertToRule "Idempotency Negate"                   "combi.idempotency.negate"                  stratIdempotencyN

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Implication Elimination strategies and rules
-------------------------------------------------------------------------------------------------------------------------------------------------
stratImplicationEliminationA = label "Strategy Implication Elimination (All Variants)"     stratImplicationElimination
stratImplicationEliminationD = label "Strategy Implication Elimination Derivative"         s
    where
        f :: SLogic -> Bool
        f (p :->: q) | hasBool p || hasBool q || skipNegations p == skipNegations q  = True
        f _                                                                          = False 
        c = evalCondOnTerm f
        s = c .*. liftToContext ruleImplicationElimination .*. 
            repeatS (somewhere stratBoolsAndDoubleNotA) .*. 
            repeatS (somewhere (stratDisjunctionsA .|. stratIdempotencyA))
stratImplicationElimination = label "Strategy Implication Elimination Single + Derivative" s
    where
        s = (ruleImplicationEliminationD |> liftToContext ruleImplicationElimination) .*. try (layerLeftMost stratDeMorganA)        
stratImplicationEliminationN = label "Strategy Implication Elimination Negate"             $ stratNegate ruleImplicationEliminationA


ruleImplicationEliminationA = convertToRule "Implication Elimination (All Variants)" "all.implicationelimination"              stratImplicationEliminationA
ruleImplicationEliminationD = convertToRule "Implication Elimination Derivative"     "combi.implicationelimination.derivative" stratImplicationEliminationD
ruleImplicationEliminationN = convertToRule "Implication Elimination Negate"         "combi.implicationelimination.negate"     stratImplicationEliminationN
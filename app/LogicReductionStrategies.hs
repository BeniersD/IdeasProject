module LogicReductionStrategies where

import Ideas.Common.Library hiding (right, layer)
import Ideas.Main.Default
import Ideas.Utils.Uniplate
import Data.List
import Domain.Logic.Formula hiding (isAnd, isOr)
import Ideas.Utils.Prelude
import LogicReductionRules
import Ideas.Common.Strategy.Traversal hiding (layer)
import Ideas.Common.Traversal.Navigator

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
-- Apply a rule multiple times somewhere
multiRuleStrategy :: Eq a => LgcRule a -> LSCtxLgc a
multiRuleStrategy x = label ("rewrite.multi." ++ show (getId x)) (repeatS (somewhere (liftToContext x)))

-- Apply of a given list of rules (choice)
multiRuleChoiceStrategy :: Eq a => [LgcRule a] -> LSCtxLgc a
multiRuleChoiceStrategy x = label description strategy
    where
        description = intercalate "-or-" (map (show . getId) x)
        strategy = choice (map liftToContext x)

------------------------------
-- Usage: repeat (layerOne (ruleDoubleNot .*. ruleDeMorganOr .*. ruleDeMorganAnd))
--- oncetd s = fix $ \x -> s |> layerOne x
------------------------------
-- left-biasedchoice
visitFirst :: (IsStrategy f, Navigator a) => f a -> Strategy a
--visitFirst :: (IsStrategy f, IsStrategy g, Navigator a) => f a -> g a -> Strategy a
--visitFirst next s = s |> next .*. (visitFirst next s) 
-- next (next layer?) vs right? 
visitFirst s = fix $ \x -> s |> (ruleRight .*. x)
--visitFirst next s = fix $ \x -> try (s |> next .*. x)

layer :: (Navigator a) => Strategy a -> Strategy a
layer s = ruleDown .*. s .*. ruleUp

layerFirst :: (IsStrategy f, Navigator a) => f a -> Strategy a
layerFirst s = layer (visitFirst s) 

isOrdered :: Ord a => Logic a -> Bool
isOrdered (p :&&: q) | p > q = True
isOrdered (p :||: q) | p > q = True
isOrdered _                  = False

isCommutativeAbsorption :: Ord a => Logic a -> Bool 
isCommutativeAbsorption ((p :&&: q) :||: r) | p == r = True
isCommutativeAbsorption (p :&&: (q :||: r)) | p == r = True
isCommutativeAbsorption (p :||: (q :&&: r)) | p == q = True
isCommutativeAbsorption ((p :||: q) :&&: r) | q == r = True
isCommutativeAbsorption _                            = False

stratCommutativityOrd :: Ord a => LSLgc a
stratCommutativityOrd = label "Commutativity-Ordered" $ check isOrdered .*. ruleCommutativity

ruleCommutativityOrdered :: Ord a => Eq a => LgcRule a
ruleCommutativityOrdered = convertToRule "Commutativity Ordered" "single.commutativity.ordered" stratCommutativityOrd

stratCommutativeAbsorption :: Ord a => LSLgc a
stratCommutativeAbsorption = label "Commutativity-Ordered" $ check isCommutativeAbsorption .*. ruleCommutativity

ruleCommutativeAbsorption :: Ord a => Eq a => LgcRule a
ruleCommutativeAbsorption = convertToRule "Commutative Absorption" "single.commutativity.absorption" stratCommutativeAbsorption

--------------------------------------------------------------------------------------------------------------------------------------
-- Simple logic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
commutativeFRuleComplement, commutativeFRuleConjunction, commutativeFRuleDisjunction, commutativeTRuleComplement,
    commutativeTRuleConjunction, commutativeTRuleDisjunction :: Ord a => Eq a => LSLgc a

commutativeFRuleComplement  = label "Commutative-and-F-Rule Complement"  $ ruleCommutativity .*. ruleFRuleComplement
commutativeFRuleConjunction = label "Commutative-and-T-Rule Complement"  $ ruleCommutativity .*. ruleFRuleConjunction
commutativeFRuleDisjunction = label "Commutative-and-F-Rule Disjunction" $ ruleCommutativity .*. ruleFRuleDisjunction
commutativeTRuleComplement  = label "Commutative-and-T-Rule Complement"  $ ruleCommutativity .*. ruleTRuleComplement
commutativeTRuleConjunction = label "Commutative-and-T-Rule Conjunction" $ ruleCommutativity .*. ruleTRuleConjunction
commutativeTRuleDisjunction = label "Commutative-and-T-Rule Disjunction" $ ruleCommutativity .*. ruleTRuleDisjunction

ruleCommutativeFRuleComplement, ruleCommutativeFRuleConjunction, ruleCommutativeFRuleDisjunction, ruleCommutativeTRuleComplement,
    ruleCommutativeTRuleConjunction, ruleCommutativeTRuleDisjunction :: Ord a => Eq a => LgcRule a

ruleCommutativeFRuleComplement  = convertToRule "Commutativity And F-Rule Complement" "single.commutativity.and.frulecomplement" commutativeFRuleComplement
ruleCommutativeFRuleConjunction = convertToRule "Commutativity And F-Rule Conjunction" "single.commutativity.and.fruleconjunction" commutativeFRuleConjunction
ruleCommutativeFRuleDisjunction = convertToRule "Commutativity And F-Rule Disjunction" "single.commutativity.and.fruledisjunction" commutativeFRuleDisjunction
ruleCommutativeTRuleComplement  = convertToRule "Commutativity And T-Rule Complement" "single.commutativity.and.trulecomplement" commutativeTRuleComplement
ruleCommutativeTRuleConjunction = convertToRule "Commutativity And T-Rule Conjunction" "single.commutativity.and.trulecomplement" commutativeTRuleConjunction
ruleCommutativeTRuleDisjunction = convertToRule "Commutativity And T-Rule Disjunction" "single.commutativity.and.truledisjunction" commutativeTRuleDisjunction

--------------------------------------------------------------------------------------------------------------------------------------
-- Advanced logic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
deMorganComplete, deMorgan, multiDeMorgan, deMorganDeriv, deMorganDeriv1, deMorganDeriv2, deMorganDeriv3, deMorganDeriv4,
    negation, multiNegation, multiDoubleNot, implicationEliminationDeriv1, implicationEliminationDeriv2, implicationEliminationDeriv3,
    implicationEliminationDeriv4, multiImplicationElimination, implicationEliminationDeriv, implicationEliminationComplete,
    mulitImplicationEliminationDeriv, multiDeMorganDeriv :: Ord a => Eq a => LSCtxLgc a

deMorgan = label "DeMorgan" $ multiRuleChoiceStrategy [ruleDeMorganOr, ruleDeMorganAnd]
negation = label "Negate" $ multiRuleChoiceStrategy [ruleTRuleNotF, ruleFRuleNotT]

multiDeMorgan = label "Multi DeMorgan" $ repeatS (somewhere deMorgan)
multiNegation = label "Multi Negate" $ repeatS (somewhere negation)
multiDoubleNot = multiRuleStrategy ruleDoubleNot

deMorganDeriv = label "DeMorgan Derivative" $ deMorganDeriv1 .|. deMorganDeriv2 .|. deMorganDeriv3 .|. deMorganDeriv4
multiDeMorganDeriv = label "Multi DeMorgan Derivative" $ repeatS (somewhere deMorganDeriv)

deMorganComplete = label "Complete DeMorgan" $ deMorganDeriv |> multiDeMorganDeriv |> multiDeMorgan

-- create new rule (draft)
-- ruleDeMorganComplete = convertToRule "DeMorgan Complete" "demorgan.complete" deMorganComplete

--deMorgan (Not (p :&&: T)) = Just (Not p :||: Not T) = Just (Not p :||: F) = Just Not p
--deMorgan (Not (T :&&: p)) = Just (Not T :||: Not p) = Just (F :||: Not p) = Just Not p
--deMorganDeriv1 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction
deMorganDeriv1 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction
--deMorganDeriv1 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction .*. multiNegation
--deMorganDeriv1 = label "DeMorgan Derivative 1" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleDisjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :&&: F)) = Just (Not p :||: Not F) = Just (Not p :||: T) = Just T
--deMorgan (Not (F :&&: p)) = Just (Not F :||: Not p) = Just (T :||: Not p) = Just T
deMorganDeriv2 = label "DeMorgan Derivative" $  innermost deMorgan .*. oncetdPref (liftToContext ruleTRuleNotF)-- .*. liftToContext ruleTRuleConjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction
--deMorganDeriv2 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction .*. multiNegation
--deMorganDeriv2 = label "DeMorgan Derivative 2" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleDisjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :||: T)) = Just (Not p :&&: Not T) = Just (Not p :&&: F) = Just F
--deMorgan (Not (T :||: p)) = Just (Not T :&&: Not p) = Just (F :&&: Not p) = Just F
deMorganDeriv3 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction
--deMorganDeriv3 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction
--deMorganDeriv3 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction .*. multiNegation
--deMorganDeriv3 = label "DeMorgan Derivative 3" $  multiDeMorgan .*. oncetdPref (liftToContext ruleFRuleNotT) .*. liftToContext ruleFRuleConjunction .*. multiNegation .*. multiDoubleNot

--deMorgan (Not (p :||: F)) = Just (Not p :&&: Not F) = Just (Not p :&&: T) = Just Not p
--deMorgan (Not (F :||: p)) = Just (Not F :&&: Not p) = Just (T :&&: Not p) = Just Not p
deMorganDeriv4 = label "DeMorgan Derivative" $  deMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction
--deMorganDeriv4 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction
--deMorganDeriv4 = label "DeMorgan Derivative" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction .*. multiNegation
--deMorganDeriv4 = label "DeMorgan Derivative 4" $  multiDeMorgan .*. oncetdPref (liftToContext ruleTRuleNotF) .*. liftToContext ruleTRuleConjunction .*. multiNegation .*. multiDoubleNot

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

evalStrategy :: LSLgc a -> LSCtxLgc a
evalStrategy x = label "eval" $ repeatS (somewhere (liftToContext x))

evalStrategy2 :: LSLgc a -> LSCtxLgc a
evalStrategy2 x = label "eval2" $ repeat1 (somewhere (liftToContext x))

evalStrategy3 :: LSLgc a -> LSCtxLgc a
evalStrategy3 x = label "eval3" $ somewhere (liftToContext x) .*. evalStrategy x

evalStrategy4 :: LSLgc a -> LSCtxLgc a
evalStrategy4 x = label "eval4" $ somewhere (liftToContext x) |> evalStrategy x

evalStrategy5 ::  LSLgc a -> LSLgc a -> LSCtxLgc a
evalStrategy5 x y = label "eval5" $ somewhere (liftToContext x) .|. evalStrategy y

evalStrategy6 :: LSLgc a -> LSCtxLgc a
evalStrategy6 x = label "eval6" $ somewhere (liftToContext x)


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
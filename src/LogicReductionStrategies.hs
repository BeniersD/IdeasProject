module LogicReductionStrategies where

import Ideas.Common.Library hiding (layer)
import Data.List
import Data.Maybe
import Domain.Logic.Formula hiding (isAnd, isOr)
import LogicReductionRules
import Ideas.Common.Traversal.Navigator
import qualified Ideas.Common.Strategy.Combinators as Combinators

--------------------------------------------------------------------------------------------------------------------------------------
-- Visits -- from Traversal.sh
--------------------------------------------------------------------------------------------------------------------------------------
data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany | VisitLeftMost | VisitRightMost

visit :: (Navigator a, IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit v next s = fix $ \a ->
   case v of
      VisitFirst     -> s  |> next .*. a
      VisitOne       -> s .|. next .*. a
      VisitSome      -> s .*. try (next .*. visit VisitMany next s) .|. next .*. a
      VisitAll       -> s .*. (Combinators.not next |> (next .*. a))
      VisitMany      -> try s .*. (Combinators.not next |> (next .*. a))
      VisitLeftMost  -> check (not.hasLeft) .*. s |> (ruleLeft .*. a)
      VisitRightMost -> check (not.hasRight) .*. s |> (ruleRight .*. a)

stratRuleMultiTerm, stratRuleMultiTerma, stratRuleMultiTerm1, stratRuleMultiTerm2 :: Eq a => Rule (Logic a) -> LabeledStrategy (Context (Logic a))
stratRuleMultiTerm r = label desc strat
    where
        desc = "Layered First " ++ showId r
        rc = liftToContext r
        v s = visit VisitMany ruleRight s
        l s = ruleDown .*. s .*. ruleUp
        strat = rc .*. l (v rc)

strattst r = label desc strat
    where
        desc = "Layered First " ++ showId r
        strat = repeatS (oncetd (stratRuleMultiTerm r))      


stratRuleMultiTerma r = label d s
    where
        d = "Layered First " ++ showId r
        lr = liftToContext r
        l s = check (not.hasDown) |> (ruleDown .*. ((s .*. l s) |> succeed) .*. ruleUp)
        s = lr .*. l (visitTryAll lr)


stratRuleMultiTerm1 r = label d s
    where
        d = "Layered First " ++ showId r
        lr = liftToContext r
        l s = check (not.hasDown) |> (ruleDown .*. ((s .*. l s) |> succeed) .*. ruleUp)
        s = lr .*. l (visitTryAll lr)

stratRuleMultiTerm2 r = label d s
    where
        d = "Layered First " ++ showId r
        lr = liftToContext r
        l s = check (not.hasDown) |> (ruleDown .*. ((s .*. l s) |> succeed) .*. ruleUp)
        s = lr .*. l (visitRightMost lr)

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
-- Apply of a given list of rules (choice)
stratMultiRuleChoice, stratMultiRuleOrElse, stratMultiRuleSeq :: [Rule (Logic a)] -> LabeledStrategy (Context (Logic a))
stratMultiRuleChoice xs = label d s
    where
        d = intercalate "-or-" (map showId xs)
        s = choice (map liftToContext xs)

-- Apply of a given list of rules (orelse)
stratMultiRuleOrElse xs = label d s
    where
        d = intercalate "-orelse-" (map showId xs)
        s = orelse (map liftToContext xs)

-- Apply of a given list of rules (sequence)
stratMultiRuleSeq xs = label d s
    where
        d = intercalate "-and-" (map showId xs)
        s = Combinators.sequence (map liftToContext xs)

-- Apply a rule once somewhere
stratRuleOnce, stratRuleAll :: Rule (Logic a) -> LabeledStrategy (Context (Logic a))
stratRuleOnce r = label ("rewrite.single." ++ showId r) (somewhere (liftToContext r))

-- Apply a rule multiple times somewhere
stratRuleAll r = label ("rewrite.multi." ++ showId r) (repeatS (stratRuleOnce r))

-- Commutative version of a rule
stratRuleC :: Eq a => Rule (Logic a) -> LabeledStrategy (Logic a)
stratRuleC r = label ("rewrite.commutative." ++ showId r) (ruleCommutativity .*. r)

stratRuleOnceC, stratRuleAllC :: Eq a => Rule (Logic a) -> LabeledStrategy (Context (Logic a))
-- Commutative version of a rule
stratRuleOnceC r = label ("rewrite.commutative." ++ showId r) (stratRuleOnce ruleCommutativity .*. liftToContext r)

-- Commutative version of a rule
stratRuleAllC r = label ("rewrite.commutative." ++ showId r) (stratRuleAll ruleCommutativity .*. liftToContext r)

--------------------------------------------------------------------------------------------------------------------------------------
-- Visits
--------------------------------------------------------------------------------------------------------------------------------------
visitFirst, visitAll, visitTryAll, visitId, visitLeftMost, visitRightMost :: (IsStrategy f, Navigator a) => f a -> Strategy a
visitAll s = fix $ \x -> s .*. (check (not.hasRight) |> (ruleRight .*. x))
visitFirst s = fix $ \x -> s |> ruleRight .*. x
visitTryAll s = fix $ \x -> try s .*. (check (not.hasRight) |> (ruleRight .*. x))
visitLeftMost s = check (not.hasDown) |> (ruleDown .*.  s)
visitRightMost s = fix $ \x -> (check (not.hasRight) .*. s) |> (ruleRight .*. x)
visitId s = check isTop |> ruleUp .*. ruleDown .*. s

--------------------------------------------------------------------------------------------------------------------------------------
-- On-layer visits
--------------------------------------------------------------------------------------------------------------------------------------
layer :: (Navigator a) => Strategy a -> Strategy a
layer s = ruleDown .*. s .*. ruleUp
layerAll s = layer (visitAll s)
layerFirst s = layer (visitFirst s)
layerLeftMost s = layer (visitLeftMost s)
layerRightMost s = layer (visitRightMost s)
layerTryAll s = layer (visitTryAll s)

--------------------------------------------------------------------------------------------------------------------------------------
-- Simple logic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
stratFRuleComplementC, stratFRuleConjunctionC, stratTRuleConjunctionC, stratTRuleComplementC,
    stratFRuleDisjunctionC, stratTRuleDisjunctionC, stratCommutativityOrd :: (Eq a, Ord a) => LabeledStrategy (Logic a)
stratFRuleComplementC  = label "Rewrite Strategy Commutative-and-F-Rule Complement"  $ check f .*. stratRuleC ruleFRuleComplement
    where 
        f :: Eq a => Logic a -> Bool
        f (Not p :&&: q) | p == q = True
        f _                       = False

stratFRuleConjunctionC = label "Rewrite Strategy Commutative-and-T-Rule Complement"  $ check f .*. stratRuleC ruleFRuleConjunction
    where 
        f :: Eq a => Logic a -> Bool
        f (F :&&: p) = True
        f _          = False

stratTRuleConjunctionC = label "Rewrite Strategy Commutative-and-T-Rule Conjunction" $ check f .*. stratRuleC ruleTRuleConjunction
    where 
        f :: Eq a => Logic a -> Bool 
        f (T :&&: p) = True
        f _          = False

stratTRuleComplementC  = label "Rewrite Strategy Commutative-and-T-Rule Complement"  $ check f .*. stratRuleC ruleTRuleComplement
    where
        f :: Eq a => Logic a -> Bool 
        f (Not p :||: q) | p == q = True
        f _                       = False

stratFRuleDisjunctionC = label "Rewrite Strategy Commutative-and-F-Rule Disjunction" $ check f .*. stratRuleC ruleFRuleDisjunction
    where 
        f :: Logic a -> Bool 
        f (F :||: p) = True
        f _          = False

stratTRuleDisjunctionC = label "Rewrite Strategy Commutative-and-T-Rule Disjunction" $ check f .*. stratRuleC ruleTRuleDisjunction
    where
        f :: Logic a -> Bool 
        f (T :||: p) = True
        f _          = False        

stratCommutativityOrd = label "Rewrite Strategy Commutativity-Ordered" $ check f |> ruleCommutativity
    where
        f :: Ord a => Logic a -> Bool
        f (p :&&: q) | p < q = True
        f (p :||: q) | p < q = True
        f _                  = False

stratDeMorgan :: Eq a => LabeledStrategy (Logic a) 
stratDeMorgan = label "Rewrite Strategy DeMorgan" $ ruleDeMorganOr .|. ruleDeMorganAnd

ruleFRuleConjunctionC, ruleTRuleConjunctionC, ruleFRuleComplementC, ruleTRuleComplementC, ruleFRuleDisjunctionC, 
    ruleTRuleDisjunctionC, ruleCommutativityOrd, ruleDeMorgan :: (Ord a) => Rule (Logic a)
ruleFRuleConjunctionC = convertToRule "Rewrite Commutative F-Rule Conjunction" "single.commutativity.and.fruleconjunction" stratFRuleConjunctionC
ruleTRuleConjunctionC = convertToRule "Rewrite Commutative T-Rule Conjunction" "single.commutativity.and.trulecomplement" stratTRuleConjunctionC
ruleFRuleComplementC  = convertToRule "Rewrite Commutative F-Rule Complement" "single.commutativity.and.frulecomplement" stratFRuleComplementC
ruleTRuleComplementC  = convertToRule "Rewrite Commutative T-Rule Complement" "single.commutativity.and.trulecomplement" stratTRuleComplementC
ruleFRuleDisjunctionC = convertToRule "Rewrite Commutative F-Rule Disjunction" "single.commutativity.and.fruledisjunction" stratFRuleDisjunctionC
ruleTRuleDisjunctionC = convertToRule "Rewrite Commutative T-Rule Disjunction" "single.commutativity.and.truledisjunction" stratTRuleDisjunctionC
ruleCommutativityOrd = convertToRule "Commutativity Ordered" "single.commutativity.ordered" stratCommutativityOrd
ruleDeMorgan = convertToRule "Rewrite De Morgan" "single.demorgan" stratDeMorgan

--------------------------------------------------------------------------------------------------------------------------------------
-- Advanced logic Strategies
--------------------------------------------------------------------------------------------------------------------------------------
isCommutativeAbsorption1, isCommutativeAbsorption2, isCommutativeAbsorption3, isCommutativeAbsorption4 :: Eq a => Logic a -> Bool 
isCommutativeAbsorption1 ((p :&&: q) :||: r) | p == r = True
isCommutativeAbsorption1 _                            = False

isCommutativeAbsorption2 (p :||: (q :&&: r)) | p == q = True
isCommutativeAbsorption2 ((p :||: q) :&&: r) | p == r = True
isCommutativeAbsorption2 ((p :||: q) :&&: r) | q == r = True
isCommutativeAbsorption2 _                            = False

isCommutativeAbsorption3 (p :||: (q :&&: r)) | p == r = True
isCommutativeAbsorption3 _                            = False

isCommutativeAbsorption4 (p :&&: (q :||: r)) | p == r = True
isCommutativeAbsorption4 _                            = False

stratCommutativeAbsorption, deMorganComplete, multiDeMorgan, deMorganDeriv, deMorganDeriv1, deMorganDeriv2, deMorganDeriv3, deMorganDeriv4,
    negation, multiNegation, multiDoubleNot, implicationEliminationDeriv1, implicationEliminationDeriv2, implicationEliminationDeriv3,
    implicationEliminationDeriv4, multiImplicationElimination, implicationEliminationDeriv, implicationEliminationComplete,
    mulitImplicationEliminationDeriv, multiDeMorganDeriv :: (Eq a, Ord a) => LabeledStrategy (Context (Logic a))

stratCommutativeAbsorption = label "Rewrite Strategy Commutativity-Absortion" s
    where
        s = (check (maybe False (not . isCommutativeAbsorption1) . fromContext) |> layer (visitLeftMost (liftToContext ruleCommutativity)) .*. liftToContext ruleAbsorption ) |>
            (check (maybe False (not . isCommutativeAbsorption2) . fromContext) |> stratMultiRuleSeq [ruleCommutativity, ruleAbsorption]) |>
            (check (maybe False (not . isCommutativeAbsorption3) . fromContext) |> liftToContext ruleCommutativity .*. layer (layerRightMost(liftToContext ruleCommutativity)) .*. liftToContext ruleAbsorption) |>
            (check (maybe False (not . isCommutativeAbsorption4) . fromContext) |> liftToContext ruleCommutativity .*. layer (layerRightMost(liftToContext ruleCommutativity)) .*. liftToContext ruleAbsorption)

-- TODO LabeledStrategy (CtxLgc a) -> Rule (Logic a)
--ruleCommutativeAbsorption :: Rule a
--ruleCommutativeAbsorption = convertToRule "Commutativity Absoption" "single.commutativity.absorption" stratCommutativeAbsorption

isOr :: Logic a -> Bool
isOr (p :||: q) = True
isOr _          = False

isAnd :: Logic a -> Bool
isAnd (p :&&: q) = True
isAnd _          = False


negation = label "Negate" $ stratMultiRuleChoice [ruleTRuleNotF, ruleFRuleNotT]

multiDeMorgan = label "Multi DeMorgan" $ repeatS (somewhere (liftToContext ruleDeMorgan))

--ruleDeMorgan :: Ord a => Eq a => LgcRule a
--ruleDeMorgan = convertToRule "De Morgan" "single.demorgan" deMorgan

multiNegation = label "Multi Negate" $ repeatS (somewhere negation)
multiDoubleNot = stratRuleAll ruleDoubleNot

deMorganDeriv = label "DeMorgan Derivative" $ deMorganDeriv1 .|. deMorganDeriv2 .|. deMorganDeriv3 .|. deMorganDeriv4
multiDeMorganDeriv = label "Multi DeMorgan Derivative" $ repeatS (somewhere deMorganDeriv)

deMorganComplete = label "Complete DeMorgan" $ deMorganDeriv |> multiDeMorganDeriv |> multiDeMorgan

-- create new rule (draft)
-- ruleDeMorganComplete = convertToRule "DeMorgan Complete" "demorgan.complete" deMorganComplete

testlf, testlta, testlta2 :: Rule (Logic a) -> LabeledStrategy (Context (Logic a))
--testlf = label "layered first" $  layerFirst (liftToContext ruleDoubleNot)
testlf x = label description strategy
    where
        description = "Layered First " ++ showId x
        strategy = layer (visitFirst (liftToContext x))

testlta x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = liftToContext x .*. layer (visitFirst (liftToContext x))

testlta2 x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = repeatS (liftToContext x .*. layer (visitFirst (liftToContext x)))

testlta3, testlmo :: LabeledStrategy (Context (Logic a)) -> LabeledStrategy (Context (Logic a))
testlta3 x = label description strategy
    where
        description = "Layered All " ++ showId x
        strategy = x .*. layer (visitAll x)

testlmo x = label description strategy
    where
        description = "Layered Left Most Only " ++ showId x
        strategy = x .*. layer (visitLeftMost x)

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

evalStrategy :: LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
evalStrategy x = label "eval" $ repeatS (somewhere (liftToContext x))

evalStrategy2 :: LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
evalStrategy2 x = label "eval2" $ repeat1 (somewhere (liftToContext x))

evalStrategy3 :: LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
evalStrategy3 x = label "eval3" $ somewhere (liftToContext x) .*. evalStrategy x

evalStrategy4 :: LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
evalStrategy4 x = label "eval4" $ somewhere (liftToContext x) |> evalStrategy x

evalStrategy5 ::  LabeledStrategy (Logic a) -> LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
evalStrategy5 x y = label "eval5" $ somewhere (liftToContext x) .|. evalStrategy y

evalStrategy6 :: LabeledStrategy (Logic a) -> LabeledStrategy (Context (Logic a))
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
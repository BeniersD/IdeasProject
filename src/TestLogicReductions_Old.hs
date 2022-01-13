module TestLogicReductions_Old (main) where

import LogicTestFunctions
import Domain.Logic.Formula
import Ideas.Common.Library hiding (layer)

main :: IO ()
main = do
    testReduction "Single Double Not" "singleDoubleNot"
    testReduction "Multiple Double Not" "multiDoubleNot"
    testReduction "Multiple De Morgan" "multiDeMorgan"
    testReduction "Multiple De Morgan and Multiple Double Negation" "multiDeMorganAndMultiDoubleNot"
    testReduction "Implication Definition" "multiImpl"
    testReduction "Multiple Implication Definition and Multiple Double Negation" "multiImplAndMultiDoubleNot"
    testReduction "Multiple Logical Equivalence" "multiLogEq"
    testReduction "Multiple Logical Equivalence and Multiple Double Negation" "multiLogEqAndMultiDoubleNot"
    testReduction "Multiple Absorption" "multiAbsorp"
    testReduction "Multiple Absorp and Multiple Double Negation" "multiAbsorpAndMultiDoubleNot"
    testReduction "Multiple Idempotentency" "multiIdemp"
    testReduction "Multiple Idempotentency and Multiple Double Negation" "multiIdempAndMultiDoubleNot"
    testReduction "Multiple Reduction F-Rule Conj" "multiFRuleConj"
    testReduction "Multiple Reduction T-Rule Conj" "multiTRuleConj"
    testReduction "Multiple Reduction F-Rule Disj" "multiTRuleDisj"
    testReduction "Multiple Reduction T-Rule Disj" "multiFRuleDisj"
    testReduction "Multiple Reduction F-Rule Compl" "multiFRuleCompl"
    testReduction "Multiple Reduction T-Rule Compl" "multiTRuleCompl"
    testReduction "Multiple Reduction F-Rule Not" "multiFRuleNot"
    testReduction "Multiple Reduction T-Rule Not" "multiTRuleNot"
    testReduction "Multiple Implication Definition and Multiple De Morgan" "multiDeMorganAndMultiImpl"
    testReduction "Multiple Implication Definition and Multiple De Morgan and Multiple Double Not" "multiDeMorganAndMultiImplAndDoubleNot"
    testReduction "Multiple Logical Equivalence and Multiple De Morgan" "multiDeMorganAndMultiLogEq"
    testReduction "Multiple Logical Equivalence and Multiple De Morgan and Multiple Double Not" "multiDeMorganAndMultiLogEqAndDoubleNot"

    testEquivalence "==" "multiDeMorganAndMultiLogEq"
    testEquivalence "==" "multiDeMorganAndMultiLogEqAndDoubleNot"
    testEquivalence "==" "multiDeMorganAndMultiImpl"
    testEquivalence "==" "multiDeMorganAndMultiImplAndDoubleNot"

    testEquivalence "(==) `on`" "multiDeMorganAndMultiLogEq"
    testEquivalence "(==) `on`" "multiDeMorganAndMultiLogEqAndDoubleNot"
    testEquivalence "(==) `on`" "multiDeMorganAndMultiImpl"
    testEquivalence "(==) `on`" "multiDeMorganAndMultiImplAndDoubleNot"

    testEquivalence "(~=)" "multiDeMorganAndMultiLogEq"
    testEquivalence "(~=)" "multiDeMorganAndMultiLogEqAndDoubleNot"
    testEquivalence "(~=)" "multiDeMorganAndMultiImpl"
    testEquivalence "(~=)" "multiDeMorganAndMultiImplAndDoubleNot"   

testReduction :: String -> String -> IO ()
testReduction x y = putStr $ "Test reduction " ++ x ++ ":\n" ++ result ++ "\n"
    where 
        test       = defineTest y
        reductions = reduceFormula test
        resultset  = zipResults reductions
        result     = resultToStr resultset

testEquivalence :: String -> String -> IO ()
testEquivalence x y = putStr $ "Test equivalence with '" ++ x ++ "':\n" ++ result ++ "\n"
--testEquivalence x y = print $ result
    where 
        testset = case defineTestSet "(==)" of 
            Just x -> x
            _ -> []
        formula = defineFunction y
        testset1 = map fst testset
        reductions1 = map formula testset1
        testset2 = map snd testset
        reductions2 = map formula testset2
        reductions = zip reductions1 reductions2
        evaluations = case x of
            "=="        -> map (\x -> if uncurry (==) x then "True" else "False") reductions
            "(==) `on`" -> map (\x -> if uncurry ((==) `on` formula) x then "True" else "False") reductions 
            "(~=)"      -> map (\x -> if uncurry (formula ~=) x then "True" else "False") testset
            _           -> ["Unknown"]
        resultset = zipResults evaluations 
        result    = unlines [ show x ++ ". " ++ show ys | (x, ys) <- resultset ]

reduceFormula :: Maybe ((SLogic -> SLogic), LabeledStrategy (SLogic)) -> [SLogic]
reduceFormula x = 
    case x of
        Just x  -> uncurry map x
        Nothing -> []

defineFunction :: String -> (SLogic -> SLogic)
defineFunction x = 
    case x of
        "singleDoubleNot"                        -> singleDoubleNot
        "multiDoubleNot"                         -> multiDoubleNot
        "multiDeMorgan"                          -> multiDeMorgan
        "multiImpl"                              -> multiImpl
        "multiImplAndMultiDoubleNot"             -> multiImplAndMultiDoubleNot
        "multiDeMorganAndMultiDoubleNot"         -> multiDeMorganAndMultiDoubleNot
        "multiLogEq"                             -> multiLogEq
        "multiLogEqAndMultiDoubleNot"            -> multiLogEqAndMultiDoubleNot
        "multiAbsorp"                            -> multiAbsorp
        "multiAbsorpAndMultiDoubleNot"           -> multiAbsorpAndMultiDoubleNot
        "multiIdemp"                             -> multiIdemp
        "multiIdempAndMultiDoubleNot"            -> multiIdempAndMultiDoubleNot
        "multiFRuleConj"                         -> multiFRuleConj
        "multiTRuleConj"                         -> multiTRuleConj
        "multiTRuleDisj"                         -> multiTRuleDisj
        "multiFRuleDisj"                         -> multiFRuleDisj
        "multiFRuleCompl"                        -> multiFRuleCompl
        "multiTRuleCompl"                        -> multiTRuleCompl
        "multiFRuleNot"                          -> multiFRuleNot
        "multiTRuleNot"                          -> multiTRuleNot
        "multiDeMorganAndMultiImpl"              -> multiDeMorganAndMultiImpl
        "multiDeMorganAndMultiImplAndDoubleNot"  -> multiDeMorganAndMultiImplAndDoubleNot
        "multiDeMorganAndMultiLogEq"             -> multiDeMorganAndMultiLogEq
        "multiDeMorganAndMultiLogEqAndDoubleNot" -> multiDeMorganAndMultiLogEqAndDoubleNot


defineTest :: String -> Maybe ((SLogic -> SLogic), [SLogic])
defineTest x = 
    case x of
        "singleDoubleNot"                        -> Just (defineFunction x, doubleNotTestSet)
        "multiDoubleNot"                         -> Just (defineFunction x, doubleNotTestSet)
        "multiDeMorgan"                          -> Just (defineFunction x, deMorganTestSet)
        "multiImpl"                              -> Just (defineFunction x, implicationTestSet)
        "multiImplAndMultiDoubleNot"             -> Just (defineFunction x, implicationTestSet)
        "multiDeMorganAndMultiDoubleNot"         -> Just (defineFunction x, deMorganAndDoubleNotTestSet)
        "multiLogEq"                             -> Just (defineFunction x, logicalEquivalenceTestSet)
        "multiLogEqAndMultiDoubleNot"            -> Just (defineFunction x, logicalEquivalenceTestSet)
        "multiAbsorp"                            -> Just (defineFunction x, absorptionTestSet)
        "multiAbsorpAndMultiDoubleNot"           -> Just (defineFunction x, absorptionTestSet)
        "multiIdemp"                             -> Just (defineFunction x, idempotencyTestSet)
        "multiIdempAndMultiDoubleNot"            -> Just (defineFunction x, idempotencyTestSet)
        "multiFRuleConj"                         -> Just (defineFunction x, boolRuleConjunctionTestSet)
        "multiTRuleConj"                         -> Just (defineFunction x, boolRuleConjunctionTestSet)
        "multiTRuleDisj"                         -> Just (defineFunction x, boolRuleDisjunctionTestSet)
        "multiFRuleDisj"                         -> Just (defineFunction x, boolRuleDisjunctionTestSet)
        "multiFRuleCompl"                        -> Just (defineFunction x, boolRuleComplementTestSet)
        "multiTRuleCompl"                        -> Just (defineFunction x, boolRuleComplementTestSet)
        "multiFRuleNot"                          -> Just (defineFunction x, boolRuleNotTestSet)
        "multiTRuleNot"                          -> Just (defineFunction x, boolRuleNotTestSet)
        "multiDeMorganAndMultiImpl"              -> Just (defineFunction x, deMorganAndImplicationTestSet)
        "multiDeMorganAndMultiImplAndDoubleNot"  -> Just (defineFunction x, deMorganAndImplicationTestSet)
        "multiDeMorganAndMultiLogEq"             -> Just (defineFunction x, deMorganAndLogicalEquivalenceTestSet)
        "multiDeMorganAndMultiLogEqAndDoubleNot" -> Just (defineFunction x, deMorganAndLogicalEquivalenceTestSet)
        _                                        -> Nothing

defineTestSet :: String -> Maybe [(SLogic, SLogic)]
defineTestSet x = 
    case x of
        "(==)" -> Just equivalenceTestSet
        _      -> Nothing
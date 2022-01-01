module LogicTestFunctions (testReduction, testEquivalence) where

import Data.Foldable as Foldable
import Data.Function
import LogicReductionRules_Old
import LogicTestCases

tupleToStr :: (Int, ChrLogic) -> [Char]
tupleToStr x = numbers ++ ". " ++ formula ++ "\n"
    where
        numbers = show $ fst x
        formula = show $ snd x

resultToStr :: [(Int, ChrLogic)] -> String
resultToStr = foldl' (++) "" . map tupleToStr

zipResults :: [a] -> [(Int, a)]
zipResults x = zip numbers x
    where
        numbers = [1..length x]

reduceFormula :: Maybe (Reduction, LChrLogic) -> [ChrLogic]
reduceFormula x = 
    case x of
        Just x  -> uncurry map x
        Nothing -> []

testReduction :: [Char] -> [Char] -> IO ()
testReduction x y = putStr $ "Test reduction " ++ x ++ ":\n" ++ result ++ "\n"
    where 
        test       = defineTest y
        reductions = reduceFormula test
        resultset  = zipResults reductions
        result     = resultToStr resultset

testEquivalence :: [Char] -> [Char] -> IO ()
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

defineFunction :: [Char] -> Reduction
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

defineTest :: [Char] -> Maybe (Reduction, LChrLogic)
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

defineTestSet :: [Char] -> Maybe [(ChrLogic, ChrLogic)]
defineTestSet x = 
    case x of
        "(==)" -> Just equivalenceTestSet
        _      -> Nothing
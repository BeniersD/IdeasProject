module TestLogicReductions_Old (main) where
import LogicTestFunctions

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
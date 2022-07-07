module TestLogicReductions (main) 
    where

import LogicReductionRules
import LogicTestCases
import LogicReductionStrategies
import LogicTestFunctions
import LogicExercices

--------------------------------------------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do    
-- Absorption rule testing -- needs attention!!
    --tstRuleAbsorption
    --tstApplyAbsorption
    --tstDerivAbsorption
    --tstRuleAbsorptionC
    --tstApplyAbsorptionC
    --tstDerivAbsorptionC
    --tstRuleAbsorptionA
    --tstApplyAbsorptionA
    --tstDerivAbsorptionA
    
-- Associativity rule testing
    --tstRuleAssociativity
    --tstApplyAssociativity
    --tstDerivAssociativity
    
-- Commutativity rule testing
    --tstRuleCommutativity
    --tstApplyCommutativity
    --tstDerivCommutativity
    --tstRuleCommutativityOrd
    --tstApplyCommutativityOrd
    --tstDerivCommutativityOrd

-- DeMorgan rule testing
    --tstRuleDeMorganOrSimple
    --tstApplyDeMorganOrSimple
    --tstDerivDeMorganOrSimple
    --tstRuleDeMorganOrComplex
    --tstApplyDeMorganOrComplex
    --tstDerivDeMorganOrComplex
    --tstRuleDeMorganAndSimple
    --tstApplyDeMorganAndSimple
    --tstDerivDeMorganAndSimple
    --tstRuleDeMorganAndComplex
    --tstApplyDeMorganAndComplex
    --tstDerivDeMorganAndComplex
    --tstRuleDeMorganSimple
    --tstApplyDeMorganSimple
    --tstDerivDeMorganSimple
    --tstRuleDeMorganComplex
    --tstApplyDeMorganComplex
    --tstDerivDeMorganComplex
    --tstRuleDeMorganAndGSimple
    --tstApplyDeMorganAndGSimple
    --tstDerivDeMorganAndGSimple
    --tstRuleDeMorganAndGComplex 
    --tstApplyDeMorganAndGComplex
    --tstDerivDeMorganAndGComplex
    --tstRuleDeMorganOrGSimple
    --tstApplyDeMorganOrGSimple
    --tstDerivDeMorganOrGSimple
    --tstRuleDeMorganOrGComplex 
    --tstApplyDeMorganOrGComplex
    --tstDerivDeMorganOrGComplex
    --tstRuleDeMorganG
    --tstApplyDeMorganG
    --tstDerivDeMorganG

-- Distributivity rule testing
    --tstRuleDistributivity
    --tstApplyDistributivity
    --tstDerivDistributivity

-- Double Not rule testing
    --tstRuleDoubleNot
    --tstApplyDoubleNot
    --tstDerivDoubleNot

-- Equivalence Elimination rule testing
    --tstRuleEquivalenceElimination
    --tstApplyEquivalenceElimination
    --tstDerivEquivalenceElimination

-- Idempotency rule testing
    --tstRuleIdempotency
    --tstApplyIdempotency
    --tstDerivIdempotency


-- Implication Elimination rule testing
    --tstRuleImplicationElimination
    --tstApplyImplicationElimination
    --tstDerivImplicationElimination

-- FRuleConjunction rule testing
    --tstRuleFRuleConjunction
    --tstApplyFRuleConjunction
    --tstDerivFRuleConjunction
    --tstRuleFRuleConjunctionC
    --tstApplyFRuleConjunctionC
    --tstDerivFRuleConjunctionC
    --tstRuleFRuleConjunctionA
    --tstApplyFRuleConjunctionA
    --tstDerivFRuleConjunctionA

-- FRuleComplement rule testing
    --tstRuleFRuleComplement
    --tstRuleFRuleComplementC
    --tstRuleFRuleComplementA

-- FRuleDisjunction rule testing
    --tstRuleFRuleDisjunction
    --tstApplyFRuleDisjunction
    --tstDerivFRuleDisjunction
    --tstRuleFRuleDisjunctionC
    --tstApplyFRuleDisjunctionC
    --tstDerivFRuleDisjunctionC
    --tstRuleFRuleDisjunctionA
    --tstApplyFRuleDisjunctionA
    --tstDerivFRuleDisjunctionA

-- FRuleNotT rule testing
    --tstRuleFRuleNotT
    --tstApplyFRuleNotT
    --tstDerivFRuleNotT

-- TRuleConjunction rule testing
    --tstRuleTRuleConjunction
    --tstApplyTRuleConjunction
    --tstDerivTRuleConjunction
    --tstRuleTRuleConjunctionC
    --tstApplyTRuleConjunction
    --tstDerivTRuleConjunction
    --tstRuleTRuleConjunctionA
    --tstApplyTRuleConjunctionA
    --tstDerivTRuleConjunctionA

-- TRuleComplement rule testing
    --tstRuleTRuleComplement
    --tstApplyTRuleComplement
    --tstDerivTRuleComplement
    --tstRuleTRuleComplementC
    --tstApplyTRuleComplementC
    --tstDerivTRuleComplementC
    --tstRuleTRuleComplementA
    tstApplyTRuleComplementA
    --tstDerivTRuleComplementA

-- TRuleDisjunction rule testing
    --tstRuleTRuleDisjunction
    --tstApplyTRuleDisjunction
    --tstDerivTRuleDisjunction
    --tstRuleTRuleDisjunctionC
    --tstApplyTRuleDisjunctionC
    --tstDerivTRuleDisjunctionC
    --tstRuleTRuleDisjunctionA
    --tstApplyTRuleDisjunctionA
    tstDerivTRuleDisjunctionA

-- TRuleNotF rule testing
    --tstRuleTRuleNotF  
    --tstApplyTRuleNotF
    --tstDerivTRuleNotF
 

--------------------------------------------------------------------------------------------------------------------------------------
-- Test functions for rules
--------------------------------------------------------------------------------------------------------------------------------------
tstRuleAbsorption, tstRuleAbsorptionC, tstRuleAbsorptionA, tstRuleAssociativity, tstRuleCommutativity, tstRuleCommutativityOrd,  
    tstRuleDeMorganAndComplex, tstRuleDeMorganAndSimple, tstRuleDeMorganOrComplex, tstRuleDeMorganOrSimple, tstRuleDeMorganComplex, 
    tstRuleDeMorganSimple, tstRuleDeMorganAndGSimple, tstRuleDeMorganAndGComplex, tstRuleDeMorganOrGSimple, tstRuleDeMorganOrGComplex,
    tstRuleDeMorganG, tstRuleDistributivity, tstRuleDoubleNot, tstRuleEquivalenceElimination, tstRuleIdempotency, 
    tstRuleImplicationElimination, tstRuleFRuleConjunction, tstRuleFRuleConjunctionC, tstRuleFRuleConjunctionA, 
    tstRuleFRuleComplement, tstRuleFRuleComplementC, tstRuleFRuleComplementA, tstRuleFRuleDisjunction, tstRuleFRuleDisjunctionC, 
    tstRuleFRuleDisjunctionA, tstRuleFRuleNotT, tstRuleTRuleConjunction, tstRuleTRuleConjunctionC, tstRuleTRuleConjunctionA, 
    tstRuleTRuleComplement, tstRuleTRuleComplementC, tstRuleTRuleComplementA, tstRuleTRuleDisjunction, tstRuleTRuleDisjunctionC, 
    tstRuleTRuleDisjunctionA, tstRuleTRuleNotF :: IO ()

-- Absorption rule testing
tstRuleAbsorption             = tstRuleGeneric     ruleAbsorption             absorptionTestSet 
tstRuleAbsorptionC            = tstStrategyGeneric stratAbsorptionC           absorptionTestSet
--tstRuleAbsorptionC            = tstStrategyGeneric ruleAbsorptionC            absorptionTestSet
tstRuleAbsorptionA            = tstStrategyGeneric stratAbsorptionA           absorptionTestSet
--tstRuleAbsorptionA            = tstStrategyGeneric ruleAbsorptionA            absorptionTestSet

-- Associativity rule testing
tstRuleAssociativity          = tstRuleGeneric     ruleAssociativity          associativityTestSet 

-- Commutativity rule testing
tstRuleCommutativity          = tstRuleGeneric     ruleCommutativity          commutativityTestSet 
tstRuleCommutativityOrd       = tstRuleGeneric     ruleCommutativityOrd       commutativityTestSet 

-- DeMorgan rule testing
tstRuleDeMorganAndComplex     = tstRuleGeneric     ruleDeMorganAnd            deMorganAndTestSetComplex 
tstRuleDeMorganAndSimple      = tstRuleGeneric     ruleDeMorganAnd            deMorganAndTestSetSimple 
tstRuleDeMorganOrComplex      = tstRuleGeneric     ruleDeMorganOr             deMorganOrTestSetComplex 
tstRuleDeMorganOrSimple       = tstRuleGeneric     ruleDeMorganOr             deMorganOrTestSetSimple 
tstRuleDeMorganComplex        = tstRuleGeneric     ruleDeMorgan               (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstRuleDeMorganSimple         = tstRuleGeneric     ruleDeMorgan               (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstRuleDeMorganAndGSimple     = tstStrategyGeneric ruleDeMorganAndG           deMorganAndTestSetSimple 
tstRuleDeMorganAndGComplex    = tstStrategyGeneric ruleDeMorganAndG           deMorganAndTestSetComplex 
tstRuleDeMorganOrGSimple      = tstStrategyGeneric ruleDeMorganOrG            deMorganOrTestSetSimple 
tstRuleDeMorganOrGComplex     = tstStrategyGeneric ruleDeMorganOrG            deMorganOrTestSetComplex 
tstRuleDeMorganG              = tstStrategyGeneric ruleDeMorganG              (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

-- Distributivity rule testing
tstRuleDistributivity         = tstRuleGeneric     ruleDistributivity         distributivityTestSet 

-- Double Not rule testing
tstRuleDoubleNot              = tstRuleGeneric     ruleDoubleNot              doubleNotTestSet 

-- Equivalence Elimination rule testing
tstRuleEquivalenceElimination = tstRuleGeneric     ruleEquivalenceElimination equivalenceEliminationTestSet 

-- Idempotency rule testing
tstRuleIdempotency            = tstRuleGeneric     ruleIdempotency            idempotencyTestSet 

-- Implication Elimination rule testing
tstRuleImplicationElimination = tstRuleGeneric     ruleImplicationElimination implicationEliminationTestSet 

-- FRuleConjunction rule testing
tstRuleFRuleConjunction       = tstRuleGeneric     ruleFRuleConjunction       boolRuleConjunctionTestSet 
tstRuleFRuleConjunctionC      = tstRuleGeneric     ruleFRuleConjunctionC      boolRuleConjunctionTestSet 
tstRuleFRuleConjunctionA      = tstRuleGeneric     ruleFRuleConjunctionA      boolRuleConjunctionTestSet 

-- FRuleComplement rule testing
tstRuleFRuleComplement        = tstRuleGeneric     ruleFRuleComplement        boolRuleComplementTestSet  
tstRuleFRuleComplementC       = tstRuleGeneric     ruleFRuleComplementC       boolRuleComplementTestSet
tstRuleFRuleComplementA       = tstRuleGeneric     ruleFRuleComplementA       boolRuleComplementTestSet

-- FRuleDisjunction rule testing
tstRuleFRuleDisjunction       = tstRuleGeneric     ruleFRuleDisjunction       boolRuleDisjunctionTestSet 
tstRuleFRuleDisjunctionC      = tstRuleGeneric     ruleFRuleDisjunctionC      boolRuleDisjunctionTestSet 
tstRuleFRuleDisjunctionA      = tstRuleGeneric     ruleFRuleDisjunctionA      boolRuleDisjunctionTestSet 

-- FRuleNotT rule testing
tstRuleFRuleNotT              = tstRuleGeneric     ruleFRuleNotT              boolRuleNotTestSet 

-- TRuleConjunction rule testing
tstRuleTRuleConjunction       = tstRuleGeneric     ruleTRuleConjunction       boolRuleConjunctionTestSet 
tstRuleTRuleConjunctionC      = tstRuleGeneric     ruleTRuleConjunctionC      boolRuleConjunctionTestSet 
tstRuleTRuleConjunctionA      = tstRuleGeneric     ruleTRuleConjunctionA      boolRuleConjunctionTestSet 

-- TRuleComplement rule testing
tstRuleTRuleComplement        = tstRuleGeneric     ruleTRuleComplement        boolRuleComplementTestSet
tstRuleTRuleComplementC       = tstRuleGeneric     ruleTRuleComplementC       boolRuleComplementTestSet 
tstRuleTRuleComplementA       = tstRuleGeneric     ruleTRuleComplementA       boolRuleComplementTestSet 

-- TRuleDisjunction rule testing
tstRuleTRuleDisjunction       = tstRuleGeneric     ruleTRuleDisjunction       boolRuleDisjunctionTestSet 
tstRuleTRuleDisjunctionC      = tstRuleGeneric     ruleTRuleDisjunctionC      boolRuleDisjunctionTestSet
tstRuleTRuleDisjunctionA      = tstRuleGeneric     ruleTRuleDisjunctionA      boolRuleDisjunctionTestSet

-- TRuleNotF rule testing
tstRuleTRuleNotF              = tstRuleGeneric     ruleTRuleNotF              boolRuleNotTestSet 

--------------------------------------------------------------------------------------------------------------------------------------
-- Test functions for apply
--------------------------------------------------------------------------------------------------------------------------------------
tstApplyAbsorption, tstApplyAbsorptionC, tstApplyAbsorptionA, tstApplyAssociativity, tstApplyCommutativity, tstApplyCommutativityOrd,  
    tstApplyDeMorganAndComplex, tstApplyDeMorganAndSimple, tstApplyDeMorganOrComplex, tstApplyDeMorganOrSimple,
    tstApplyDeMorganComplex, tstApplyDeMorganSimple, tstApplyDeMorganAndGSimple, tstApplyDeMorganAndGComplex,
    tstApplyDeMorganOrGSimple, tstApplyDeMorganOrGComplex, tstApplyDeMorganG, tstApplyDistributivity, tstApplyDoubleNot, 
    tstApplyEquivalenceElimination, tstApplyIdempotency, tstApplyImplicationElimination, tstApplyFRuleConjunction,  
    tstApplyFRuleConjunctionC, tstApplyFRuleConjunctionA, tstApplyFRuleComplement, tstApplyFRuleComplementC, tstApplyFRuleComplementA, 
    tstApplyFRuleDisjunction, tstApplyFRuleDisjunctionC, tstApplyFRuleDisjunctionA, tstApplyFRuleNotT, tstApplyTRuleConjunction, 
    tstApplyTRuleConjunctionC, tstApplyTRuleConjunctionA, tstApplyTRuleComplement, tstApplyTRuleComplementC, tstApplyTRuleComplementA,  
    tstApplyTRuleDisjunction, tstApplyTRuleDisjunctionC, tstApplyTRuleDisjunctionA, tstApplyTRuleNotF :: IO ()

-- Absorption rule testing
tstApplyAbsorption             = tstApply ruleAbsorption             SomeWhereRepeat absorptionTestSet
tstApplyAbsorptionC            = tstApply stratAbsorptionC           SomeWhereRepeat absorptionTestSet
--tstApplyAbsorptionC            = tstApply ruleAbsorptionC            SomeWhereRepeat absorptionTestSet
tstApplyAbsorptionA            = tstApply stratAbsorptionA           SomeWhereRepeat absorptionTestSet
--tstApplyAbsorptionA            = tstApply ruleAbsorptionA            SomeWhereRepeat absorptionTestSet

-- Associativity rule testing
tstApplyAssociativity          = tstApply ruleAssociativity          SomeWhereRepeat associativityTestSet

-- Commutativity rule testing
tstApplyCommutativity          = tstApply ruleCommutativity          SomeWhereRepeat commutativityTestSet 
tstApplyCommutativityOrd       = tstApply ruleCommutativityOrd       SomeWhereRepeat commutativityTestSet 

-- DeMorgan rule testing
tstApplyDeMorganAndComplex     = tstApply ruleDeMorganAnd            SomeWhereRepeat deMorganAndTestSetComplex 
tstApplyDeMorganAndSimple      = tstApply ruleDeMorganAnd            SomeWhereRepeat deMorganAndTestSetSimple 
tstApplyDeMorganOrComplex      = tstApply ruleDeMorganOr             SomeWhereRepeat deMorganOrTestSetComplex 
tstApplyDeMorganOrSimple       = tstApply ruleDeMorganOr             SomeWhereRepeat deMorganOrTestSetSimple 
tstApplyDeMorganComplex        = tstApply ruleDeMorgan               SomeWhereRepeat (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstApplyDeMorganSimple         = tstApply ruleDeMorgan               SomeWhereRepeat (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstApplyDeMorganAndGSimple     = tstApply ruleDeMorganAndG           SomeWhereRepeat deMorganAndTestSetSimple 
tstApplyDeMorganAndGComplex    = tstApply ruleDeMorganAndG           SomeWhereRepeat deMorganAndTestSetComplex 
tstApplyDeMorganOrGSimple      = tstApply ruleDeMorganOrG            SomeWhereRepeat deMorganOrTestSetSimple 
tstApplyDeMorganOrGComplex     = tstApply ruleDeMorganOrG            SomeWhereRepeat deMorganOrTestSetComplex 
tstApplyDeMorganG              = tstApply ruleDeMorganG              SomeWhereRepeat (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

-- Distributivity rule testing
tstApplyDistributivity         = tstApply ruleDistributivity         SomeWhereRepeat distributivityTestSet 

-- Double Not rule testing
tstApplyDoubleNot              = tstApply ruleDoubleNot              SomeWhereRepeat doubleNotTestSet 

-- Equivalence Elimination rule testing
tstApplyEquivalenceElimination = tstApply ruleEquivalenceElimination SomeWhereRepeat equivalenceEliminationTestSet 

-- Idempotency rule testing
tstApplyIdempotency            = tstApply ruleIdempotency            SomeWhereRepeat idempotencyTestSet 

-- Implication Elimination rule testing
tstApplyImplicationElimination = tstApply ruleImplicationElimination SomeWhereRepeat implicationEliminationTestSet 

-- FRuleConjunction rule testing
tstApplyFRuleConjunction       = tstApply ruleFRuleConjunction       SomeWhereRepeat boolRuleConjunctionTestSet 
tstApplyFRuleConjunctionC      = tstApply ruleFRuleConjunctionC      SomeWhereRepeat boolRuleConjunctionTestSet 
tstApplyFRuleConjunctionA      = tstApply ruleFRuleConjunctionA      SomeWhereRepeat boolRuleConjunctionTestSet 

-- FRuleComplement rule testing
tstApplyFRuleComplement        = tstApply ruleFRuleComplement        SomeWhereRepeat boolRuleComplementTestSet  
tstApplyFRuleComplementC       = tstApply ruleFRuleComplementC       SomeWhereRepeat boolRuleComplementTestSet
tstApplyFRuleComplementA       = tstApply ruleFRuleComplementA       SomeWhereRepeat boolRuleComplementTestSet

-- FRuleDisjunction rule testing
tstApplyFRuleDisjunction       = tstApply ruleFRuleDisjunction       SomeWhereRepeat boolRuleDisjunctionTestSet 
tstApplyFRuleDisjunctionC      = tstApply ruleFRuleDisjunctionC      SomeWhereRepeat boolRuleDisjunctionTestSet 
tstApplyFRuleDisjunctionA      = tstApply ruleFRuleDisjunctionA      SomeWhereRepeat boolRuleDisjunctionTestSet 

-- FRuleNotT rule testing
tstApplyFRuleNotT              = tstApply ruleFRuleNotT              SomeWhereRepeat boolRuleNotTestSet 

-- TRuleConjunction rule testing
tstApplyTRuleConjunction       = tstApply ruleTRuleConjunction       SomeWhereRepeat boolRuleConjunctionTestSet 
tstApplyTRuleConjunctionC      = tstApply ruleTRuleConjunctionC      SomeWhereRepeat boolRuleConjunctionTestSet 
tstApplyTRuleConjunctionA      = tstApply ruleTRuleConjunctionA      SomeWhereRepeat boolRuleConjunctionTestSet 

-- TRuleComplement rule testing
tstApplyTRuleComplement        = tstApply ruleTRuleComplement        SomeWhereRepeat boolRuleComplementTestSet
tstApplyTRuleComplementC       = tstApply ruleTRuleComplementC       SomeWhereRepeat boolRuleComplementTestSet 
tstApplyTRuleComplementA       = tstApply ruleTRuleComplementA       SomeWhereRepeat boolRuleComplementTestSet 

-- TRuleDisjunction rule testing
tstApplyTRuleDisjunction       = tstApply ruleTRuleDisjunction       SomeWhereRepeat boolRuleDisjunctionTestSet 
tstApplyTRuleDisjunctionC      = tstApply ruleTRuleDisjunctionC      SomeWhereRepeat boolRuleDisjunctionTestSet
tstApplyTRuleDisjunctionA      = tstApply ruleTRuleDisjunctionA      SomeWhereRepeat boolRuleDisjunctionTestSet

-- TRuleNotF rule testing
tstApplyTRuleNotF              = tstApply ruleTRuleNotF              SomeWhereRepeat boolRuleNotTestSet 

--------------------------------------------------------------------------------------------------------------------------------------
-- Test functions for derivations
--------------------------------------------------------------------------------------------------------------------------------------
tstDerivAbsorption, tstDerivAbsorptionC, tstDerivAbsorptionA, tstDerivAssociativity, tstDerivCommutativity, tstDerivCommutativityOrd,  
    tstDerivDeMorganAndComplex, tstDerivDeMorganAndSimple, tstDerivDeMorganOrComplex, tstDerivDeMorganOrSimple,
    tstDerivDeMorganComplex, tstDerivDeMorganSimple, tstDerivDeMorganAndGSimple, tstDerivDeMorganAndGComplex,
    tstDerivDeMorganOrGSimple, tstDerivDeMorganOrGComplex, tstDerivDeMorganG, tstDerivDistributivity, tstDerivDoubleNot, 
    tstDerivEquivalenceElimination, tstDerivIdempotency, tstDerivImplicationElimination, tstDerivFRuleConjunction,  
    tstDerivFRuleConjunctionC, tstDerivFRuleConjunctionA, tstDerivFRuleComplement, tstDerivFRuleComplementC, tstDerivFRuleComplementA, 
    tstDerivFRuleDisjunction, tstDerivFRuleDisjunctionC, tstDerivFRuleDisjunctionA, tstDerivFRuleNotT, tstDerivTRuleConjunction, 
    tstDerivTRuleConjunctionC, tstDerivTRuleConjunctionA, tstDerivTRuleComplement, tstDerivTRuleComplementC, tstDerivTRuleComplementA,  
    tstDerivTRuleDisjunction, tstDerivTRuleDisjunctionC, tstDerivTRuleDisjunctionA, tstDerivTRuleNotF :: IO ()


-- Absorption rule testing
tstDerivAbsorption             = tstDerivation ruleAbsorption             SomeWhereRepeat absorptionTestSet
tstDerivAbsorptionC            = tstDerivation stratAbsorptionC           SomeWhereRepeat absorptionTestSet
--tstDerivAbsorptionC            = tstDerivation ruleAbsorptionC            SomeWhereRepeat absorptionTestSet
tstDerivAbsorptionA            = tstDerivation stratAbsorptionA           SomeWhereRepeat absorptionTestSet
--tstDerivAbsorptionA            = tstDerivation ruleAbsorptionA            SomeWhereRepeat absorptionTestSet

-- Associativity rule testing
tstDerivAssociativity          = tstDerivation (ruleToStrategy ruleAssociativity)     SomeWhereRepeat associativityTestSet

-- Commutativity rule testing
tstDerivCommutativity          = tstDerivation ruleCommutativity          SomeWhereRepeat commutativityTestSet 
tstDerivCommutativityOrd       = tstDerivation ruleCommutativityOrd       SomeWhereRepeat commutativityTestSet 

-- DeMorgan rule testing
tstDerivDeMorganAndComplex     = tstDerivation ruleDeMorganAnd            SomeWhereRepeat deMorganAndTestSetComplex 
tstDerivDeMorganAndSimple      = tstDerivation ruleDeMorganAnd            SomeWhereRepeat deMorganAndTestSetSimple 
tstDerivDeMorganOrComplex      = tstDerivation ruleDeMorganOr             SomeWhereRepeat deMorganOrTestSetComplex 
tstDerivDeMorganOrSimple       = tstDerivation ruleDeMorganOr             SomeWhereRepeat deMorganOrTestSetSimple 
tstDerivDeMorganComplex        = tstDerivation ruleDeMorgan               SomeWhereRepeat (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstDerivDeMorganSimple         = tstDerivation ruleDeMorgan               SomeWhereRepeat (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstDerivDeMorganAndGSimple     = tstDerivation ruleDeMorganAndG           SomeWhereRepeat deMorganAndTestSetSimple 
tstDerivDeMorganAndGComplex    = tstDerivation ruleDeMorganAndG           SomeWhereRepeat deMorganAndTestSetComplex 
tstDerivDeMorganOrGSimple      = tstDerivation ruleDeMorganOrG            SomeWhereRepeat deMorganOrTestSetSimple 
tstDerivDeMorganOrGComplex     = tstDerivation ruleDeMorganOrG            SomeWhereRepeat deMorganOrTestSetComplex 
tstDerivDeMorganG              = tstDerivation ruleDeMorganG              SomeWhereRepeat (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

-- Distributivity rule testing
tstDerivDistributivity         = tstDerivation ruleDistributivity         SomeWhereRepeat distributivityTestSet 

-- Double Not rule testing
tstDerivDoubleNot              = tstDerivation ruleDoubleNot              SomeWhereRepeat doubleNotTestSet 

-- Equivalence Elimination rule testing
tstDerivEquivalenceElimination = tstDerivation ruleEquivalenceElimination SomeWhereRepeat equivalenceEliminationTestSet 

-- Idempotency rule testing
tstDerivIdempotency            = tstDerivation ruleIdempotency            SomeWhereRepeat idempotencyTestSet 

-- Implication Elimination rule testing
tstDerivImplicationElimination = tstDerivation ruleImplicationElimination SomeWhereRepeat implicationEliminationTestSet 

-- FRuleConjunction rule testing
tstDerivFRuleConjunction       = tstDerivation ruleFRuleConjunction       SomeWhereRepeat boolRuleConjunctionTestSet 
tstDerivFRuleConjunctionC      = tstDerivation ruleFRuleConjunctionC      SomeWhereRepeat boolRuleConjunctionTestSet 
tstDerivFRuleConjunctionA      = tstDerivation ruleFRuleConjunctionA      SomeWhereRepeat boolRuleConjunctionTestSet 

-- FRuleComplement rule testing
tstDerivFRuleComplement        = tstDerivation ruleFRuleComplement        SomeWhereRepeat boolRuleComplementTestSet  
tstDerivFRuleComplementC       = tstDerivation ruleFRuleComplementC       SomeWhereRepeat boolRuleComplementTestSet
tstDerivFRuleComplementA       = tstDerivation ruleFRuleComplementA       SomeWhereRepeat boolRuleComplementTestSet

-- FRuleDisjunction rule testing
tstDerivFRuleDisjunction       = tstDerivation ruleFRuleDisjunction       SomeWhereRepeat boolRuleDisjunctionTestSet 
tstDerivFRuleDisjunctionC      = tstDerivation ruleFRuleDisjunctionC      SomeWhereRepeat boolRuleDisjunctionTestSet 
tstDerivFRuleDisjunctionA      = tstDerivation ruleFRuleDisjunctionA      SomeWhereRepeat boolRuleDisjunctionTestSet 

-- FRuleNotT rule testing
tstDerivFRuleNotT              = tstDerivation ruleFRuleNotT              SomeWhereRepeat boolRuleNotTestSet 

-- TRuleConjunction rule testing
tstDerivTRuleConjunction       = tstDerivation ruleTRuleConjunction       SomeWhereRepeat boolRuleConjunctionTestSet 
tstDerivTRuleConjunctionC      = tstDerivation ruleTRuleConjunctionC      SomeWhereRepeat boolRuleConjunctionTestSet 
tstDerivTRuleConjunctionA      = tstDerivation ruleTRuleConjunctionA      SomeWhereRepeat boolRuleConjunctionTestSet 

-- TRuleComplement rule testing
tstDerivTRuleComplement        = tstDerivation ruleTRuleComplement        SomeWhereRepeat boolRuleComplementTestSet
tstDerivTRuleComplementC       = tstDerivation ruleTRuleComplementC       SomeWhereRepeat boolRuleComplementTestSet 
tstDerivTRuleComplementA       = tstDerivation ruleTRuleComplementA       SomeWhereRepeat boolRuleComplementTestSet 

-- TRuleDisjunction rule testing
tstDerivTRuleDisjunction       = tstDerivation ruleTRuleDisjunction       SomeWhereRepeat boolRuleDisjunctionTestSet 
tstDerivTRuleDisjunctionC      = tstDerivation ruleTRuleDisjunctionC      SomeWhereRepeat boolRuleDisjunctionTestSet
tstDerivTRuleDisjunctionA      = tstDerivation ruleTRuleDisjunctionA      SomeWhereRepeat boolRuleDisjunctionTestSet

-- TRuleNotF rule testing
tstDerivTRuleNotF              = tstDerivation ruleTRuleNotF              SomeWhereRepeat boolRuleNotTestSet 
module TestLogicReductions (main) 
    where

import LogicReductionRules
import LogicTestCases
import LogicReductionStrategies
import LogicTestFunctions
import LogicExercices
import LogicConstants


import Ideas.Common.Library
import LogicFunctions
import Domain.Logic.Formula
import Ideas.Common.Traversal.Navigator
import qualified Ideas.Common.Strategy.Combinators as Combinators 
--------------------------------------------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do    
    --tstApply      stratDoubleNot          SomeWhereRepeatS layerTestSet 
    --tstDerivation stratDoubleNot          SomeWhereRepeatS layerTestSet 
    --tstApply      stratLayerDoubleNot          SomeWhereRepeatS layerTestSet 
    --tstDerivation stratDoubleNot          SomeWhereRepeatS layerTestSet
    --tstDerivation stratDerivDeMorgan      SomeWhere (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex) 
    --tstDerivation stratDerivLayerDeMorgan          SomeWhere (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex) 
    --  tstDerivation stratLayerTFRuleNotTF SomeWhereRepeatS layerTestSet 
      tstDerivation stratLayerUnary       Single layerTestSet 

-- Absorption rule testing
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
    --tstApplyTRuleComplementA
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
    --tstDerivTRuleDisjunctionA

-- TRuleNotF rule testing
    --tstRuleTRuleNotF  
    --tstApplyTRuleNotF
    --tstDerivTRuleNotF
 

-- Negative terms
    --tstDerivation stratNegTerms           SomeWhereRepeatS negTermsTestSet


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
tstApplyAbsorption             = tstApply ruleAbsorption             SomeWhereRepeatS absorptionTestSet
tstApplyAbsorptionC            = tstApply stratAbsorptionC           SomeWhereRepeatS absorptionTestSet
--tstApplyAbsorptionC            = tstApply ruleAbsorptionC            SomeWhereRepeatS absorptionTestSet
tstApplyAbsorptionA            = tstApply stratAbsorptionA           SomeWhereRepeatS absorptionTestSet
--tstApplyAbsorptionA            = tstApply ruleAbsorptionA            SomeWhereRepeatS absorptionTestSet

-- Associativity rule testing
tstApplyAssociativity          = tstApply ruleAssociativity          SomeWhereRepeatS associativityTestSet

-- Commutativity rule testing
tstApplyCommutativity          = tstApply ruleCommutativity          SomeWhereRepeatS commutativityTestSet 
tstApplyCommutativityOrd       = tstApply ruleCommutativityOrd       SomeWhereRepeatS commutativityTestSet 

-- DeMorgan rule testing
tstApplyDeMorganAndComplex     = tstApply ruleDeMorganAnd            SomeWhereRepeatS deMorganAndTestSetComplex 
tstApplyDeMorganAndSimple      = tstApply ruleDeMorganAnd            SomeWhereRepeatS deMorganAndTestSetSimple 
tstApplyDeMorganOrComplex      = tstApply ruleDeMorganOr             SomeWhereRepeatS deMorganOrTestSetComplex 
tstApplyDeMorganOrSimple       = tstApply ruleDeMorganOr             SomeWhereRepeatS deMorganOrTestSetSimple 
tstApplyDeMorganComplex        = tstApply ruleDeMorgan               SomeWhereRepeatS (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstApplyDeMorganSimple         = tstApply ruleDeMorgan               SomeWhereRepeatS (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstApplyDeMorganAndGSimple     = tstApply ruleDeMorganAndG           SomeWhereRepeatS deMorganAndTestSetSimple 
tstApplyDeMorganAndGComplex    = tstApply ruleDeMorganAndG           SomeWhereRepeatS deMorganAndTestSetComplex 
tstApplyDeMorganOrGSimple      = tstApply ruleDeMorganOrG            SomeWhereRepeatS deMorganOrTestSetSimple 
tstApplyDeMorganOrGComplex     = tstApply ruleDeMorganOrG            SomeWhereRepeatS deMorganOrTestSetComplex 
tstApplyDeMorganG              = tstApply ruleDeMorganG              SomeWhereRepeatS (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

-- Distributivity rule testing
tstApplyDistributivity         = tstApply ruleDistributivity         SomeWhereRepeatS distributivityTestSet 

-- Double Not rule testing
tstApplyDoubleNot              = tstApply ruleDoubleNot              SomeWhereRepeatS doubleNotTestSet 

-- Equivalence Elimination rule testing
tstApplyEquivalenceElimination = tstApply ruleEquivalenceElimination SomeWhereRepeatS equivalenceEliminationTestSet 

-- Idempotency rule testing
tstApplyIdempotency            = tstApply ruleIdempotency            SomeWhereRepeatS idempotencyTestSet 

-- Implication Elimination rule testing
tstApplyImplicationElimination = tstApply ruleImplicationElimination SomeWhereRepeatS implicationEliminationTestSet 

-- FRuleConjunction rule testing
tstApplyFRuleConjunction       = tstApply ruleFRuleConjunction       SomeWhereRepeatS boolRuleConjunctionTestSet 
tstApplyFRuleConjunctionC      = tstApply ruleFRuleConjunctionC      SomeWhereRepeatS boolRuleConjunctionTestSet 
tstApplyFRuleConjunctionA      = tstApply ruleFRuleConjunctionA      SomeWhereRepeatS boolRuleConjunctionTestSet 

-- FRuleComplement rule testing
tstApplyFRuleComplement        = tstApply ruleFRuleComplement        SomeWhereRepeatS boolRuleComplementTestSet  
tstApplyFRuleComplementC       = tstApply ruleFRuleComplementC       SomeWhereRepeatS boolRuleComplementTestSet
tstApplyFRuleComplementA       = tstApply ruleFRuleComplementA       SomeWhereRepeatS boolRuleComplementTestSet

-- FRuleDisjunction rule testing
tstApplyFRuleDisjunction       = tstApply ruleFRuleDisjunction       SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstApplyFRuleDisjunctionC      = tstApply ruleFRuleDisjunctionC      SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstApplyFRuleDisjunctionA      = tstApply ruleFRuleDisjunctionA      SomeWhereRepeatS boolRuleDisjunctionTestSet 

-- FRuleNotT rule testing
tstApplyFRuleNotT              = tstApply ruleFRuleNotT              SomeWhereRepeatS boolRuleNotTestSet 

-- TRuleConjunction rule testing
tstApplyTRuleConjunction       = tstApply ruleTRuleConjunction       SomeWhereRepeatS boolRuleConjunctionTestSet 
tstApplyTRuleConjunctionC      = tstApply ruleTRuleConjunctionC      SomeWhereRepeatS boolRuleConjunctionTestSet 
tstApplyTRuleConjunctionA      = tstApply ruleTRuleConjunctionA      SomeWhereRepeatS boolRuleConjunctionTestSet 

-- TRuleComplement rule testing
tstApplyTRuleComplement        = tstApply ruleTRuleComplement        SomeWhereRepeatS boolRuleComplementTestSet
tstApplyTRuleComplementC       = tstApply ruleTRuleComplementC       SomeWhereRepeatS boolRuleComplementTestSet 
tstApplyTRuleComplementA       = tstApply ruleTRuleComplementA       SomeWhereRepeatS boolRuleComplementTestSet 

-- TRuleDisjunction rule testing
tstApplyTRuleDisjunction       = tstApply ruleTRuleDisjunction       SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstApplyTRuleDisjunctionC      = tstApply ruleTRuleDisjunctionC      SomeWhereRepeatS boolRuleDisjunctionTestSet
tstApplyTRuleDisjunctionA      = tstApply ruleTRuleDisjunctionA      SomeWhereRepeatS boolRuleDisjunctionTestSet

-- TRuleNotF rule testing
tstApplyTRuleNotF              = tstApply ruleTRuleNotF              SomeWhereRepeatS boolRuleNotTestSet 

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
tstDerivAbsorption             = tstDerivation ruleAbsorption             SomeWhereRepeatS absorptionTestSet
tstDerivAbsorptionC            = tstDerivation stratAbsorptionC           SomeWhereRepeatS absorptionTestSet
--tstDerivAbsorptionC            = tstDerivation ruleAbsorptionC            SomeWhereRepeatS absorptionTestSet
tstDerivAbsorptionA            = tstDerivation stratAbsorptionA           SomeWhereRepeatS absorptionTestSet
--tstDerivAbsorptionA            = tstDerivation ruleAbsorptionA            SomeWhereRepeatS absorptionTestSet

-- Associativity rule testing
tstDerivAssociativity          = tstDerivation (ruleToStrategy ruleAssociativity)     SomeWhereRepeatS associativityTestSet

-- Commutativity rule testing
tstDerivCommutativity          = tstDerivation ruleCommutativity          SomeWhereRepeatS commutativityTestSet 
tstDerivCommutativityOrd       = tstDerivation ruleCommutativityOrd       SomeWhereRepeatS commutativityTestSet 

-- DeMorgan rule testing
tstDerivDeMorganAndComplex     = tstDerivation ruleDeMorganAnd            SomeWhereRepeatS deMorganAndTestSetComplex 
tstDerivDeMorganAndSimple      = tstDerivation ruleDeMorganAnd            SomeWhereRepeatS deMorganAndTestSetSimple 
tstDerivDeMorganOrComplex      = tstDerivation ruleDeMorganOr             SomeWhereRepeatS deMorganOrTestSetComplex 
tstDerivDeMorganOrSimple       = tstDerivation ruleDeMorganOr             SomeWhereRepeatS deMorganOrTestSetSimple 
tstDerivDeMorganComplex        = tstDerivation ruleDeMorgan               SomeWhereRepeatS (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstDerivDeMorganSimple         = tstDerivation ruleDeMorgan               SomeWhereRepeatS (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstDerivDeMorganAndGSimple     = tstDerivation ruleDeMorganAndG           SomeWhereRepeatS deMorganAndTestSetSimple 
tstDerivDeMorganAndGComplex    = tstDerivation ruleDeMorganAndG           SomeWhereRepeatS deMorganAndTestSetComplex 
tstDerivDeMorganOrGSimple      = tstDerivation ruleDeMorganOrG            SomeWhereRepeatS deMorganOrTestSetSimple 
tstDerivDeMorganOrGComplex     = tstDerivation ruleDeMorganOrG            SomeWhereRepeatS deMorganOrTestSetComplex 
tstDerivDeMorganG              = tstDerivation ruleDeMorganG              SomeWhereRepeatS (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

-- Distributivity rule testing
tstDerivDistributivity         = tstDerivation ruleDistributivity         SomeWhereRepeatS distributivityTestSet 

-- Double Not rule testing
tstDerivDoubleNot              = tstDerivation ruleDoubleNot              SomeWhereRepeatS doubleNotTestSet 

-- Equivalence Elimination rule testing
tstDerivEquivalenceElimination = tstDerivation ruleEquivalenceElimination SomeWhereRepeatS equivalenceEliminationTestSet 

-- Idempotency rule testing
tstDerivIdempotency            = tstDerivation ruleIdempotency            SomeWhereRepeatS idempotencyTestSet 

-- Implication Elimination rule testing
tstDerivImplicationElimination = tstDerivation ruleImplicationElimination SomeWhereRepeatS implicationEliminationTestSet 

-- FRuleConjunction rule testing
tstDerivFRuleConjunction       = tstDerivation ruleFRuleConjunction       SomeWhereRepeatS boolRuleConjunctionTestSet 
tstDerivFRuleConjunctionC      = tstDerivation ruleFRuleConjunctionC      SomeWhereRepeatS boolRuleConjunctionTestSet 
tstDerivFRuleConjunctionA      = tstDerivation ruleFRuleConjunctionA      SomeWhereRepeatS boolRuleConjunctionTestSet 

-- FRuleComplement rule testing
tstDerivFRuleComplement        = tstDerivation ruleFRuleComplement        SomeWhereRepeatS boolRuleComplementTestSet  
tstDerivFRuleComplementC       = tstDerivation ruleFRuleComplementC       SomeWhereRepeatS boolRuleComplementTestSet
tstDerivFRuleComplementA       = tstDerivation ruleFRuleComplementA       SomeWhereRepeatS boolRuleComplementTestSet

-- FRuleDisjunction rule testing
tstDerivFRuleDisjunction       = tstDerivation ruleFRuleDisjunction       SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstDerivFRuleDisjunctionC      = tstDerivation ruleFRuleDisjunctionC      SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstDerivFRuleDisjunctionA      = tstDerivation ruleFRuleDisjunctionA      SomeWhereRepeatS boolRuleDisjunctionTestSet 

-- FRuleNotT rule testing
tstDerivFRuleNotT              = tstDerivation ruleFRuleNotT              SomeWhereRepeatS boolRuleNotTestSet 

-- TRuleConjunction rule testing
tstDerivTRuleConjunction       = tstDerivation ruleTRuleConjunction       SomeWhereRepeatS boolRuleConjunctionTestSet 
tstDerivTRuleConjunctionC      = tstDerivation ruleTRuleConjunctionC      SomeWhereRepeatS boolRuleConjunctionTestSet 
tstDerivTRuleConjunctionA      = tstDerivation ruleTRuleConjunctionA      SomeWhereRepeatS boolRuleConjunctionTestSet 

-- TRuleComplement rule testing
tstDerivTRuleComplement        = tstDerivation ruleTRuleComplement        SomeWhereRepeatS boolRuleComplementTestSet
tstDerivTRuleComplementC       = tstDerivation ruleTRuleComplementC       SomeWhereRepeatS boolRuleComplementTestSet 
tstDerivTRuleComplementA       = tstDerivation ruleTRuleComplementA       SomeWhereRepeatS boolRuleComplementTestSet 

-- TRuleDisjunction rule testing
tstDerivTRuleDisjunction       = tstDerivation ruleTRuleDisjunction       SomeWhereRepeatS boolRuleDisjunctionTestSet 
tstDerivTRuleDisjunctionC      = tstDerivation ruleTRuleDisjunctionC      SomeWhereRepeatS boolRuleDisjunctionTestSet
tstDerivTRuleDisjunctionA      = tstDerivation ruleTRuleDisjunctionA      SomeWhereRepeatS boolRuleDisjunctionTestSet

-- TRuleNotF rule testing
tstDerivTRuleNotF              = tstDerivation ruleTRuleNotF              SomeWhereRepeatS boolRuleNotTestSet 
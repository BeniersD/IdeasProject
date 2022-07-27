module TestLogicReductions (main) 
    where

import Domain.Logic.Formula
import Ideas.Common.Library
import LogicReductionRules
import LogicTestCases
import LogicReductionStrategies
import LogicTestFunctions
import LogicExercices
import LogicConstants
import LogicFunctions


-------------------------------------------------------------------------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do    

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Combinated strategies
-------------------------------------------------------------------------------------------------------------------------------------------------
    --printDerivations   (evalExercise (evalStrategy ruleAC Single)) (commutativityTestSet!!45)
    --tstApply           stratAC                      SomeWhereRepeat1  commutativityTestSet 
    --tstStrategyGeneric stratAC                      commutativityTestSet 
    --tstDerivation      stratAC                      SomeWhereRepeat1  commutativityTestSet 

    --tstApply           ruleAC                       SomeWhereRepeat1  commutativityTestSet 
    --tstStrategyGeneric ruleAC                       commutativityTestSet 
    --tstDerivation      ruleAC                       SomeWhereRepeat1    commutativityTestSet 

    --tstApply           stratACI                     SomeWhereRepeat1  commutativityTestSet 
    --tstStrategyGeneric stratACI                     commutativityTestSet 
    --tstDerivation      stratACI                     SomeWhere         commutativityTestSet 

    --tstApply           stratUnairies                SomeWhereRepeat1  layerTestSet 
    --tstStrategyGeneric stratUnairies                layerTestSet 
    --tstDerivation      stratUnairies                SomeWhereRepeat1  layerTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- F-Rule Not T/T-Rule Not F variants
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleMultiTFRuleNotTF         SomeWhere         layerTestSet 
    --tstStrategyGeneric ruleMultiTFRuleNotTF         layerTestSet 
    --tstDerivation      ruleMultiTFRuleNotTF         SomeWhere         layerTestSet

    --tstApply           stratTFRuleNotTFUnary        SomeWhere         layerTestSet 
    --tstStrategyGeneric stratTFRuleNotTFUnary        layerTestSet 
    --tstDerivation      stratTFRuleNotTFUnary        SomeWhere         layerTestSet
 
    --tstApply           ruleLayerTFRuleNotTF         SomeWhere         layerTestSet 
    --tstStrategyGeneric ruleLayerTFRuleNotTF         layerTestSet 
    --tstDerivation      ruleLayerTFRuleNotTF         SomeWhere         layerTestSet

    --tstApply           ruleTFRuleNotTFA              SomeWhere         layerTestSet 
    --tstStrategyGeneric ruleTFRuleNotTFA              layerTestSet 
    --tstDerivation      ruleTFRuleNotTFA              SomeWhere         layerTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Absorption rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleAbsorption               SomeWhereRepeat1 absorptionTestSet
    --tstRuleGeneric     ruleAbsorption               absorptionTestSet 
    --tstDerivation      ruleAbsorption               SomeWhereRepeat1 absorptionTestSet

    --tstApply           stratAbsorptionC             SomeWhereRepeat1 absorptionTestSet
    --tstStrategyGeneric stratAbsorptionC             absorptionTestSet
    --tstDerivation      stratAbsorptionC             SomeWhereRepeat1 absorptionTestSet

    --tstApply           ruleAbsorptionC              SomeWhereRepeat1 absorptionTestSet
    --tstStrategyGeneric ruleAbsorptionC              absorptionTestSet
    --tstDerivation      ruleAbsorptionC              SomeWhereRepeat1 absorptionTestSet

    --tstApply           stratAbsorptionD             SomeWhere absorptionTestSet
    --tstStrategyGeneric stratAbsorptionD             absorptionTestSet
    --tstDerivation      stratAbsorptionD             SomeWhere absorptionTestSet

    --tstApply           stratAbsorptionA             SomeWhereRepeat1 absorptionTestSet
    --tstStrategyGeneric stratAbsorptionA             absorptionTestSet
    tstDerivation      stratAbsorptionA             SomeWhereRepeat1 absorptionTestSet

    --tstApply           ruleAbsorptionA              SomeWhereRepeat1 absorptionTestSet
    --tstStrategyGeneric ruleAbsorptionA              absorptionTestSet
    --tstDerivation      ruleAbsorptionA              SomeWhereRepeat1 absorptionTestSet
 
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Associativity rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleAssociativity            SomeWhereRepeat1 associativityTestSet
    --tstRuleGeneric     ruleAssociativity            associativityTestSet 
    --tstDerivation      ruleAssociativity            SomeWhereRepeat1 associativityTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Commutativity rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleCommutativity            SomeWhere        commutativityTestSet 
    --tstRuleGeneric     ruleCommutativity            commutativityTestSet 
    --tstDerivation      ruleCommutativity            SomeWhere        commutativityTestSet 

    --tstApply           ruleCommutativityOrd         SomeWhereRepeat1 commutativityTestSet 
    --tstRuleGeneric     ruleCommutativityOrd         commutativityTestSet 
    --tstDerivation      ruleCommutativityOrd         SomeWhereRepeat1 commutativityTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- DeMorgan rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleDeMorganAnd              SomeWhereRepeat1 deMorganAndTestSetSimple 
    --tstRuleGeneric     ruleDeMorganAnd              deMorganAndTestSetSimple 
    --tstDerivation      ruleDeMorganAnd              SomeWhereRepeat1 deMorganAndTestSetSimple 

    --tstApply           ruleDeMorganAnd              SomeWhereRepeat1 deMorganAndTestSetComplex 
    --tstRuleGeneric     ruleDeMorganAnd              deMorganAndTestSetComplex 
    --tstDerivation      ruleDeMorganAnd              SomeWhereRepeat1 deMorganAndTestSetComplex 

    --tstApply           ruleDeMorganOr               SomeWhereRepeat1 deMorganOrTestSetSimple 
    --tstRuleGeneric     ruleDeMorganOr               deMorganOrTestSetSimple 
    --tstDerivation      ruleDeMorganOr               SomeWhereRepeat1 deMorganOrTestSetSimple 

    --tstApply           ruleDeMorgan                 SomeWhereRepeat1 (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
    --tstRuleGeneric     ruleDeMorgan                 (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
    --tstDerivation      ruleDeMorgan                 SomeWhereRepeat1 (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 

    --tstApply           ruleDeMorganOr               SomeWhereRepeat1 deMorganOrTestSetComplex 
    --tstRuleGeneric     ruleDeMorganOr               deMorganOrTestSetComplex 
    --tstDerivation      ruleDeMorganOr               SomeWhereRepeat1 deMorganOrTestSetComplex 

    --tstApply           ruleDeMorgan                 SomeWhereRepeat1 (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
    --tstRuleGeneric     ruleDeMorgan                 (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
    --tstDerivation      ruleDeMorgan                 SomeWhereRepeat1 (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 

    --tstApply           ruleDeMorganG                SomeWhereRepeat1 (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)
    --tstStrategyGeneric ruleDeMorganG                (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)
    --tstDerivation      ruleDeMorganG                SomeWhereRepeat1 (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex)

    --tstApply           ruleDeMorganAndG             SomeWhere        deMorganAndTestSetSimple 
    --tstStrategyGeneric ruleDeMorganAndG             deMorganAndTestSetSimple 
    --tstDerivation      ruleDeMorganAndG             SomeWhere        deMorganAndTestSetSimple 

    --tstApply           ruleDeMorganAndG             SomeWhere        deMorganAndTestSetComplex 
    --tstStrategyGeneric ruleDeMorganAndG             deMorganAndTestSetComplex
    --tstDerivation      ruleDeMorganAndG             SomeWhere        deMorganAndTestSetComplex 
 
    --tstApply           ruleDeMorganOrG              SomeWhereRepeat1 deMorganOrTestSetSimple 
    --tstStrategyGeneric ruleDeMorganOrG              deMorganOrTestSetSimple 
    --tstDerivation      ruleDeMorganOrG              SomeWhereRepeat1 deMorganOrTestSetSimple 

    --tstApply           ruleDeMorganOrG              SomeWhereRepeat1 deMorganOrTestSetComplex 
    --tstStrategyGeneric ruleDeMorganOrG              deMorganOrTestSetComplex 
    --tstDerivation      ruleDeMorganOrG              SomeWhereRepeat1 deMorganOrTestSetComplex 

    --tstApply           stratDeMorganD               SomeWhereRepeat1 deMorganDerivTestSet 
    --tstStrategyGeneric stratDeMorganD               deMorganDerivTestSet 
    --tstDerivation      stratDeMorganD               SomeWhereRepeat1 deMorganDerivTestSet

    --tstApply           stratDeMorganD               SomeWhereRepeat1 (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex) 
    --tstStrategyGeneric stratDeMorganD               (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex) 
    --tstDerivation      stratDeMorganD               SomeWhereRepeat1 (deMorganOrTestSetSimple ++ deMorganAndTestSetComplex) 
    
    --tstApply           stratDeMorganA               SomeWhereRepeat1 deMorganDerivTestSet 
    --tstStrategyGeneric stratDeMorganA               deMorganDerivTestSet 
    --tstDerivation      stratDeMorganA               SomeWhere deMorganDerivTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Distributivity rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleDistributivity           SomeWhere        distributivityTestSet 
    --tstRuleGeneric     ruleDistributivity           distributivityTestSet 
    --tstDerivation      ruleDistributivity           SomeWhere        distributivityTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Double Not rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --putStrLn           $ show (isDoubleNot (Not (Not F)))
    --putStrLn           $ show (isDoubleNot (Not (Not (Not F))))

    --tstApply           ruleDoubleNot                SomeWhereRepeat1 doubleNotTestSet 
    --tstRuleGeneric     ruleDoubleNot                doubleNotTestSet 
    --tstDerivation      ruleDoubleNot                SomeWhereRepeat1 doubleNotTestSet 

    --tstApply           stratDoubleNotUnary          SomeWhereRepeat1 layerTestSet 
    --tstStrategyGeneric stratDoubleNotUnary          layerTestSet
    --tstDerivation      stratDoubleNotUnary          SomeWhere layerTestSet

    --tstApply           stratLayerDoubleNot          SomeWhere        layerTestSet
    --tstStrategyGeneric stratLayerDoubleNot          layerTestSet 
    --tstDerivation      stratLayerDoubleNot          SomeWhere        layerTestSet  

    --tstApply           stratDoubleNot               SomeWhereRepeat1 layerTestSet 
    --tstStrategyGeneric stratDoubleNot               layerTestSet
    --tstDerivation      stratDoubleNot               SomeWhereRepeat1 layerTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equivalence Elimination rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleEquivalenceElimination   SomeWhereRepeat1 equivalenceEliminationTestSet 
    --tstRuleGeneric     ruleEquivalenceElimination   equivalenceEliminationTestSet 
    --tstDerivation      ruleEquivalenceElimination   SomeWhereRepeat1 equivalenceEliminationTestSet 

    --tstApply           stratEquivalenceEliminationD SomeWhereRepeat1 (layerTestSet ++ equivalenceEliminationDerivTestSet) 
    --tstStrategyGeneric stratEquivalenceEliminationD (layerTestSet ++ equivalenceEliminationDerivTestSet) 
    --tstDerivation      stratEquivalenceEliminationD SomeWhere  (layerTestSet ++ equivalenceEliminationDerivTestSet)

    --tstApply           stratEquivalenceEliminationN SomeWhereRepeat1 layerTestSet 
    --tstStrategyGeneric stratEquivalenceEliminationN (layerTestSet ++ equivalenceEliminationDerivTestSet) 
    --tstDerivation      stratEquivalenceEliminationN SomeWhere  layerTestSet


    --tstApply           stratEquivalenceEliminationA SomeWhere (equivalenceEliminationDerivTestSet ++ layerTestSet) 
    --tstStrategyGeneric stratEquivalenceEliminationA (equivalenceEliminationDerivTestSet ++ layerTestSet) 
    --tstDerivation      stratEquivalenceEliminationA SomeWhere  (equivalenceEliminationDerivTestSet ++ layerTestSet)
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Idempotency rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleIdempotency              SomeWhereRepeat1 idempotencyTestSet
    --tstRuleGeneric     ruleIdempotency              idempotencyTestSet 
    --tstDerivation      ruleIdempotency              SomeWhereRepeat1 idempotencyTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Implication Elimination rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleImplicationElimination   SomeWhereRepeat1 (layerTestSet ++ implicationEliminationDerivTestSet) 
    --tstRuleGeneric     ruleImplicationElimination   implicationEliminationTestSet 
    --tstDerivation      ruleImplicationElimination   SomeWhereRepeat1 (layerTestSet ++ implicationEliminationDerivTestSet) 

    --putStrLn $ show 
    --tstApply           stratImplicationEliminationD SomeWhereRepeat1 (layerTestSet ++ implicationEliminationDerivTestSet) 
    --tstStrategyGeneric stratImplicationEliminationD (layerTestSet ++ implicationEliminationDerivTestSet) 
    --tstDerivation      stratImplicationEliminationD SomeWhere        (layerTestSet ++ implicationEliminationDerivTestSet)

    --tstApply           stratImplicationEliminationN SomeWhereRepeat1 (layerTestSet ++ implicationEliminationDerivTestSet) 
    --tstStrategyGeneric stratImplicationEliminationN (layerTestSet ++ implicationEliminationDerivTestSet) 
    --tstDerivation      stratImplicationEliminationN SomeWhere        (layerTestSet ++ implicationEliminationDerivTestSet)

    --tstApply           stratImplicationEliminationA SomeWhere        implicationEliminationDerivTestSet 
    --tstStrategyGeneric stratImplicationEliminationA implicationEliminationDerivTestSet
    --tstDerivation      stratImplicationEliminationA SomeWhere        implicationEliminationDerivTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- FRuleConjunction rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleFRuleConjunction         SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleFRuleConjunction         boolRuleConjunctionTestSet 
    --tstDerivation      ruleFRuleConjunction         SomeWhereRepeat1 boolRuleConjunctionTestSet 

    --tstApply           ruleFRuleConjunctionC        SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleFRuleConjunctionC        boolRuleConjunctionTestSet 
    --tstDerivation      ruleFRuleConjunctionC        SomeWhereRepeat1 boolRuleConjunctionTestSet 

    --tstApply           ruleFRuleConjunctionA        SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleFRuleConjunctionA        boolRuleConjunctionTestSet
    --tstDerivation      stratFRuleConjunctionA       SomeWhereRepeat1 boolRuleConjunctionTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- FRuleComplement rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleFRuleComplement          SomeWhereRepeat1 boolRuleComplementTestSet  
    --tstRuleGeneric     ruleFRuleComplement          boolRuleComplementTestSet  
    --tstDerivation      ruleFRuleComplement          SomeWhereRepeat1 boolRuleComplementTestSet  

    --tstApply           ruleFRuleComplementC         SomeWhereRepeat1 boolRuleComplementTestSet
    --tstRuleGeneric     ruleFRuleComplementC         boolRuleComplementTestSet
    --tstDerivation      ruleFRuleComplementC         SomeWhereRepeat1 boolRuleComplementTestSet

    --tstApply           ruleFRuleComplementA         SomeWhereRepeat1 boolRuleComplementTestSet
    --tstRuleGeneric     ruleFRuleComplementA         boolRuleComplementTestSet
    --tstDerivation      ruleFRuleComplementA         SomeWhereRepeat1 boolRuleComplementTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- FRuleDisjunction rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleFRuleDisjunction         SomeWhereRepeat1 boolRuleDisjunctionTestSet 
    --tstRuleGeneric     ruleFRuleDisjunction         boolRuleDisjunctionTestSet 
    --tstDerivation      ruleFRuleDisjunction         SomeWhereRepeat1 boolRuleDisjunctionTestSet 

    --tstApply           ruleFRuleDisjunctionC        SomeWhereRepeat1 boolRuleDisjunctionTestSet 
    --tstRuleGeneric     ruleFRuleDisjunctionC        boolRuleDisjunctionTestSet 
    --tstDerivation      ruleFRuleDisjunctionC        SomeWhereRepeat1 boolRuleDisjunctionTestSet 

    --tstApply           ruleFRuleDisjunctionA        SomeWhereRepeat1 boolRuleDisjunctionTestSet 
    --tstRuleGeneric     ruleFRuleDisjunctionA        boolRuleDisjunctionTestSet 
    --tstDerivation      ruleFRuleDisjunctionA        SomeWhereRepeat1 boolRuleDisjunctionTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- FRuleNotT rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleFRuleNotT                SomeWhereRepeat1 boolRuleNotTestSet 
    --tstRuleGeneric     ruleFRuleNotT                boolRuleNotTestSet 
    --tstDerivation      ruleFRuleNotT                SomeWhereRepeat1 boolRuleNotTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- TRuleConjunction rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleTRuleConjunction         SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleTRuleConjunction         boolRuleConjunctionTestSet 
    --tstDerivation      ruleTRuleConjunction         SomeWhereRepeat1 boolRuleConjunctionTestSet 

    --tstApply           ruleTRuleConjunctionC        SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleTRuleConjunctionC        boolRuleConjunctionTestSet 
    --tstDerivation      ruleTRuleConjunctionC        SomeWhereRepeat1 boolRuleConjunctionTestSet 

    --tstApply           ruleTRuleConjunctionA        SomeWhereRepeat1 boolRuleConjunctionTestSet 
    --tstRuleGeneric     ruleTRuleConjunctionA        boolRuleConjunctionTestSet 
    --tstDerivation      ruleTRuleConjunctionA        SomeWhereRepeat1 boolRuleConjunctionTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- TRuleComplement rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleTRuleComplement          SomeWhereRepeat1 boolRuleComplementTestSet
    --tstRuleGeneric     ruleTRuleComplement          boolRuleComplementTestSet
    --tstDerivation      ruleTRuleComplement          SomeWhereRepeat1 boolRuleComplementTestSet

    --tstApply           ruleTRuleComplementC         SomeWhereRepeat1 boolRuleComplementTestSet 
    --tstRuleGeneric     ruleTRuleComplementC         boolRuleComplementTestSet 
    --tstDerivation      ruleTRuleComplementC         SomeWhereRepeat1 boolRuleComplementTestSet 

    --tstApply           ruleTRuleComplementA         SomeWhereRepeat1 boolRuleComplementTestSet 
    --tstRuleGeneric     ruleTRuleComplementA         boolRuleComplementTestSet 
    --tstDerivation      ruleTRuleComplementA         SomeWhereRepeat1 boolRuleComplementTestSet    

-------------------------------------------------------------------------------------------------------------------------------------------------
-- TRuleDisjunction rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleTRuleDisjunction         SomeWhereRepeat1 boolRuleDisjunctionTestSet 
    --tstRuleGeneric     ruleTRuleDisjunction         boolRuleDisjunctionTestSet 
    --tstDerivation      ruleTRuleDisjunction         SomeWhereRepeat1 boolRuleDisjunctionTestSet 

    --tstApply           ruleTRuleDisjunctionC        SomeWhereRepeat1 boolRuleDisjunctionTestSet
    --tstRuleGeneric     ruleTRuleDisjunctionC        boolRuleDisjunctionTestSet
    --tstDerivation      ruleTRuleDisjunctionC        SomeWhereRepeat1 boolRuleDisjunctionTestSet

    --tstApply           ruleTRuleDisjunctionA        SomeWhereRepeat1 boolRuleDisjunctionTestSet
    --tstRuleGeneric     ruleTRuleDisjunctionA        boolRuleDisjunctionTestSet
    --tstDerivation      ruleTRuleDisjunctionA        SomeWhereRepeat1 boolRuleDisjunctionTestSet

-------------------------------------------------------------------------------------------------------------------------------------------------
-- TRuleNotF rule testing
-------------------------------------------------------------------------------------------------------------------------------------------------
    --tstApply           ruleTRuleNotF                SomeWhereRepeat1 boolRuleNotTestSet 
    --tstDerivation      ruleTRuleNotF                SomeWhereRepeat1 boolRuleNotTestSet 
    --tstRuleGeneric     ruleTRuleNotF                boolRuleNotTestSet 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Negative terms
-------------------------------------------------------------------------------------------------------------------------------------------------
--tstDerivation          stratNegTerms                SomeWhereRepeat1 negTermsTestSet
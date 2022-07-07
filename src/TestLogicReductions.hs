module TestLogicReductions (main) 
    where

import Domain.Logic.Formula
import LogicConstants
import LogicReductionRules
import LogicTestCases
import LogicReductionStrategies
import LogicTestFunctions
import Ideas.Common.Exercise
import Ideas.Common.Library
import LogicExercices

quickTestSet :: [SLogic]
quickTestSet = [
                Not (p :&&: q),                                   -- ¬(p ˄ q)
                Not (q :&&: p :&&: r),                            -- ¬(p ˄ q ˄ r)
                Not (q :&&: p :&&: r :&&: s),                     -- ¬(p ˄ q ˄ r ˄ s)
                Not (q :&&: r :&&: s :&&: p),                     -- ¬(p ˄ r ˄ s ˄ p)                
                Not (q :&&: p :&&: r :&&: s :&&: t),              -- ¬(p ˄ q ˄ r ˄ s ˄ t)
                --Not (Not (p :&&: q))                             -- ¬¬(p ˄ q)
                --Not (Not (q :&&: p)),                             -- ¬¬(q ˄ p)  
                --Not (Not (q :&&: r :&&: p)),                      -- ¬¬(q ˄ r ˄ p)                
                Not (Not (Not p) :&&: T),                       -- ¬(¬¬p ˄ T) 
                Not (Not (Not p) :&&: T :&&: T),                -- ¬(¬¬p ˄ T ˄ T)
                Not (Not (Not p) :&&: T :&&: F),                -- ¬(¬¬p ˄ T ˄ F)                
                Not (Not (Not p) :&&: T :&&: Not (Not p)),    -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                Not (Not (Not p) :&&: T :&&: Not (Not q)),    -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                Not (Not (Not p) :&&: Not p :&&: T)           -- ¬(¬¬p ˄ ¬p ˄ T)
               ]

quickTestSet2 :: [SLogic]
quickTestSet2 = [
                p :&&: Not (p :&&: q),                             
                p :&&: Not (q :&&: p),                               
                p :&&: Not (q :&&: r :&&: p),                          
                p :&&: Not (q :&&: p :&&: r :&&: s :&&: t),          
                p :&&: (p :&&: Not (q :&&: p :&&: r :&&: s :&&: t))                    
               ]

--------------------------------------------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do    
    -- Absorption rule testing -- needs attention!!
    --tstRuleAbsorption
    --tstDerivAbsorption
    --tstRuleAbsorptionC
    --tstDerivAbsorptionC
    --tstRuleAbsorptionA
    --tstDerivAbsorptionA
    
    
    
    -- Associativity rule testing
    --tstRuleAssociativity
    tstApplyAssociativity
    --tstDerivAssociativity
    
    

    -- Commutativity rule testing
    --tstRuleCommutativity


    --tstRuleCommutativityOrd

   
    -- DeMorgan rule testing
    --tstRuleDeMorganOrSimple
    --tstRuleDeMorganOrComplex
    --tstRuleDeMorganAndSimple
    --tstRuleDeMorganAndComplex
    --tstRuleDeMorganSimple
    --tstRuleDeMorganComplex
    --tstRuleDeMorganAndGSimple
    --tstRuleDeMorganAndGComplex 
    --tstRuleDeMorganOrGSimple
    --tstRuleDeMorganOrGComplex 
    --tstRuleDeMorganG

    -- Distributivity rule testing
    --tstRuleDistributivity

    -- Double Not rule testing
    --tstRuleDoubleNot

    -- Equivalence Elimination rule testing
    --tstRuleEquivalenceElimination

    -- Idempotency rule testing
    --tstRuleIdempotency

    -- Implication Elimination rule testing
    --tstRuleImplicationElimination

    -- FRuleConjunction rule testing
    --tstRuleFRuleConjunction
    --tstRuleFRuleConjunctionC
    --tstRuleFRuleConjunctionA

    -- FRuleComplement rule testing
    --tstRuleFRuleComplement
    --tstRuleFRuleComplementC
    --tstRuleFRuleComplementA

    -- FRuleDisjunction rule testing
    --tstRuleFRuleDisjunction
    --tstRuleFRuleDisjunctionC
    --tstRuleFRuleDisjunctionA

    -- FRuleNotT rule testing
    --tstRuleFRuleNotT

    -- TRuleConjunction rule testing
    --tstRuleTRuleConjunction
    --tstRuleTRuleConjunctionC
    --tstRuleTRuleConjunctionA

    -- TRuleComplement rule testing
    --tstRuleTRuleComplement
    --tstRuleTRuleComplementC
    --tstRuleTRuleComplementA

    -- TRuleDisjunction rule testing
    --tstRuleTRuleDisjunction
    --tstRuleTRuleDisjunctionC
    --tstRuleTRuleDisjunctionA

    -- TRuleNotF rule testing
    --tstRuleTRuleNotF   

--------------------------------------------------------------------------------------------------------------------------------------
-- Test functions for rules
--------------------------------------------------------------------------------------------------------------------------------------
tstRuleAbsorption, tstRuleAssociativity, tstRuleCommutativity, tstRuleCommutativityOrd, tstRuleDeMorganAndComplex, 
    tstRuleDeMorganAndSimple, tstRuleDeMorganOrComplex, tstRuleDeMorganOrSimple, tstRuleDeMorganComplex, 
    tstRuleDeMorganSimple, tstRuleDoubleNot, tstRuleEquivalenceElimination, tstRuleIdempotency, tstRuleImplicationElimination, 
    tstRuleFRuleComplement, tstRuleTRuleComplement, tstRuleFRuleConjunctionC, tstRuleFRuleConjunction, tstRuleTRuleConjunction,
    tstRuleTRuleConjunctionC, tstRuleFRuleComplementC, tstRuleTRuleComplementC, tstRuleFRuleDisjunctionC,
    tstRuleTRuleDisjunctionC, tstRuleFRuleDisjunction, tstRuleTRuleDisjunction, tstRuleFRuleNotT, tstRuleTRuleNotF,
    tstRuleDistributivity, tstRuleAbsorptionC, tstRuleAbsorptionA :: IO ()
-- Absorption rule testing
tstRuleAbsorption             = tstRuleGeneric     ruleAbsorption             absorptionTestSet 
tstRuleAbsorptionC            = tstStrategyGeneric stratAbsorptionC           absorptionTestSet
tstRuleAbsorptionA            = tstStrategyGeneric stratAbsorptionA           absorptionTestSet

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
tstApplyAbsorptionA :: IO ()

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
tstDerivAbsorption, tstDerivAbsorptionC, tstDerivAbsorptionA :: IO ()

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
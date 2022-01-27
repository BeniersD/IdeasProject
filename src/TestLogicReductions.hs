module TestLogicReductions (main) where

import Ideas.Common.Library
import Ideas.Main.Default
import Domain.Logic.Formula 
import LogicReductionRules
import LogicReductionStrategies
import LogicExercices
import LogicTestCases
import LogicTestFunctions

quickTestSet :: [Logic String]
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

quickTestSet2 :: [Logic String]
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
    --tstRuleDeMorganOrSimple
    --tstRuleDeMorganOrComplex
    --tstRuleDeMorganAndSimple
    --tstRuleDeMorganAndComplex
    --tstRuleDeMorganSimple
    --tstRuleDeMorganComplex
    --tstRuleCommutativity
    --tstRuleCommutativityOrd
    --tstRuleAbsorption
    --tstRuleDoubleNot
    --tstRuleEquivalenceElimination
    --tstRuleIdempotency
    --tstRuleImplicationElimination
    --tstRuleFRuleComplement
    --tstRuleTRuleComplement
    --tstRuleFRuleConjunction
    --tstRuleTRuleConjunction
    --tstRuleFRuleDisjunction
    --tstRuleTRuleDisjunction
    --tstRuleFRuleNotT
    --tstRuleTRuleNotF
    --tstRuleDistributivity
    --tstRuleFRuleConjunctionC
    --tstRuleTRuleConjunctionC
    --tstRuleFRuleComplementC
    --tstRuleTRuleComplementC
    --tstRuleFRuleDisjunctionC
    --tstRuleTRuleDisjunctionC
    --mapM_ print quickTestSet
    mapM_ print quickTestSet2
    --pptest "Test Layer Top All" [applyD (stratRuleMultiTerm1 ruleDeMorgan) $ newContext $ termNavigator x | x <- quickTestSet ] 
    --pptest "Test Layer Top All" [applyD (stratRuleMultiTerma ruleDeMorgan) $ newContext $ termNavigator x | x <- quickTestSet ] 
    pptest "Test Layer Top All" [applyD (stratRuleMultiTerm ruleDeMorgan) $ newContext $ termNavigator x | x <- quickTestSet2 ] 
    pptest "Test Layer Top All" [applyD (strattst ruleDeMorgan) $ newContext $ termNavigator x | x <- quickTestSet2 ] 
    

--------------------------------------------------------------------------------------------------------------------------------------
-- Test Functions
--------------------------------------------------------------------------------------------------------------------------------------
tstRuleAbsorption, tstRuleAssociativity, tstRuleCommutativity, tstRuleCommutativityOrd, tstRuleDeMorganAndComplex, 
    tstRuleDeMorganAndSimple, tstRuleDeMorganOrComplex, tstRuleDeMorganOrSimple, tstRuleDeMorganComplex, 
    tstRuleDeMorganSimple, tstRuleEquivalenceElimination, tstRuleIdempotency, tstRuleFRuleConjunctionC,
    tstRuleTRuleConjunctionC, tstRuleFRuleComplementC, tstRuleTRuleComplementC, tstRuleFRuleDisjunctionC,
    tstRuleTRuleDisjunctionC  :: IO ()
tstRuleAbsorption = tstRuleGeneric ruleAbsorption absorptionTestSet 
tstRuleAssociativity = tstRuleGeneric ruleAssociativity associativityTestSet 
tstRuleCommutativity = tstRuleGeneric ruleCommutativity commutativityTestSet 
tstRuleCommutativityOrd = tstRuleGeneric ruleCommutativityOrd commutativityTestSet 
tstRuleDeMorganAndComplex = tstRuleGeneric ruleDeMorganAnd deMorganAndTestSetComplex 
tstRuleDeMorganAndSimple = tstRuleGeneric ruleDeMorganAnd deMorganAndTestSetSimple 
tstRuleDeMorganOrComplex = tstRuleGeneric ruleDeMorganOr deMorganOrTestSetComplex 
tstRuleDeMorganOrSimple = tstRuleGeneric ruleDeMorganOr deMorganOrTestSetSimple 
tstRuleDeMorganComplex = tstRuleGeneric ruleDeMorgan (deMorganAndTestSetComplex ++ deMorganOrTestSetComplex) 
tstRuleDeMorganSimple = tstRuleGeneric ruleDeMorgan (deMorganAndTestSetSimple ++ deMorganOrTestSetSimple) 
tstRuleDoubleNot = tstRuleGeneric ruleDoubleNot doubleNotTestSet 
tstRuleEquivalenceElimination = tstRuleGeneric ruleEquivalenceElimination equivalenceEliminationTestSet 
tstRuleIdempotency = tstRuleGeneric ruleIdempotency idempotencyTestSet 
tstRuleImplicationElimination = tstRuleGeneric ruleImplicationElimination implicationEliminationTestSet 
tstRuleFRuleComplement = tstRuleGeneric ruleFRuleComplement boolRuleComplementTestSet 
tstRuleTRuleComplement = tstRuleGeneric ruleTRuleComplement boolRuleComplementTestSet 
tstRuleFRuleConjunction = tstRuleGeneric ruleFRuleConjunction boolRuleConjunctionTestSet 
tstRuleTRuleConjunction = tstRuleGeneric ruleTRuleConjunction boolRuleConjunctionTestSet 
tstRuleFRuleDisjunction = tstRuleGeneric ruleFRuleDisjunction boolRuleDisjunctionTestSet 
tstRuleTRuleDisjunction = tstRuleGeneric ruleTRuleDisjunction boolRuleDisjunctionTestSet 
tstRuleFRuleNotT = tstRuleGeneric ruleFRuleNotT boolRuleNotTestSet 
tstRuleTRuleNotF = tstRuleGeneric ruleTRuleNotF boolRuleNotTestSet 
tstRuleDistributivity = tstRuleGeneric ruleDistributivity distributivityTestSet 
tstRuleFRuleConjunctionC = tstRuleGeneric ruleFRuleConjunctionC boolRuleConjunctionTestSet 
tstRuleTRuleConjunctionC = tstRuleGeneric ruleTRuleConjunctionC boolRuleConjunctionTestSet 
tstRuleFRuleComplementC = tstRuleGeneric ruleFRuleComplementC boolRuleComplementTestSet 
tstRuleTRuleComplementC = tstRuleGeneric ruleTRuleComplementC boolRuleComplementTestSet 
tstRuleFRuleDisjunctionC = tstRuleGeneric ruleFRuleDisjunctionC boolRuleDisjunctionTestSet 
tstRuleTRuleDisjunctionC = tstRuleGeneric ruleTRuleDisjunctionC boolRuleDisjunctionTestSet 

--tstRuleTRuleDisjunctionC2 = tstRuleGeneric stratRuleAllC(ruleTRuleDisjunctionC) boolRuleDisjunctionTestSet



    --pptest "deMorganDerivTestSet" deMorganDerivTestSet
    --pptest "DeMorgan Strategy" [applyD (deMorgan) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 
    --pptest "Test Layer Top All" [applyD (testlta ruleDeMorganAnd) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    -- putStrLn "\n"    
    --mapM_ print $ [apply (checkStrategy) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 
    --mapM_ print $ [apply (layerFirst  lift ruleCommutativity) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet]     
    --mapM_ print $ [apply (testlf ruleDeMorganAnd) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    -- putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleChoiceStrategy [ruleDeMorganAnd, ruleDeMorganOr, ruleTRuleNotF])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    --putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleChoiceStrategy [ruleDeMorganAnd, ruleDeMorganOr, ruleFRuleNotT, ruleTRuleNotF])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    --putStrLn "\nT-Rule Complement testSet:"
    --mapM_ print boolRuleComplementTestSet

    --putStrLn "\nruleTRuleComplement:"
    --mapM_ print $ map (applyD ruleTRuleComplement) boolRuleComplementTestSet   

    --putStrLn "\nruleCommutativeTRuleComplement:"
    --mapM_ print $ map (applyD ruleCommutativeTRuleComplement) boolRuleComplementTestSet   


{--
    putStrLn "\nDeMorgan Deriv TestSet:"
    mapM_ print deMorganDerivTestSet

    

    putStrLn "\nMulti DeMorgan Strategy:"
    mapM_ print $ [apply (multiDeMorgan) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    putStrLn "\nDeMorgan Derivative Strategy:"
    mapM_ print $ [apply (deMorganDeriv) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    putStrLn "\nComplete DeMorgan Strategy:"
    mapM_ print $ [apply (deMorganComplete) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    putStrLn "\nImplication Elimination Deriv TestSet:"
    mapM_ print implicationEliminationDerivTestSet

    putStrLn "\nImplication Elimination Strategy:"
    mapM_ print $ [apply (liftToContext ruleImplicationElimination) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 

    putStrLn "\nMulti Implication Elimination Strategy:"
    mapM_ print $ [apply (multiImplicationElimination) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 

    putStrLn "\nImplication Elimination Derivative Strategy"
    mapM_ print $ [apply (implicationEliminationDeriv) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 

    putStrLn "\nMulti Implication Elimination Derivative Strategy"
    mapM_ print $ [apply (mulitImplicationEliminationDeriv) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 

    putStrLn "\nComplete Implication Elimination Derivative Strategy:"
    mapM_ print $ [apply (implicationEliminationComplete) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 
    Test code:   
    putStrLn "hasRule / applyRule / applyRuleD doubleNotRule:"
    print $ map (hasRule doubleNotRule) samples
    print $ map (applyRule doubleNotRule) samples
    print $ map (applyRuleD doubleNotRule) samples

    putStrLn "\nApply minimalExercise:"
    print $ map (apply (minimalExercise doubleNotOrDeMorgan)) samples
    print $ map (applyD (minimalExercise doubleNotOrDeMorgan)) samples
    print $ map (applyAll (minimalExercise doubleNotOrDeMorgan)) samples    

    --putStrLn "\nMultirule commutativity:"
    --mapM_ print $ [apply (multiRule2Strategy ruleCommutativity) $ newContext $ termNavigator x | x <- samples]

--    putStrLn "\ndeMorganDerivative2:"
--    mapM_ print $ [apply (deMorganDerivative) $ newContext $ termNavigator x | x <- samples] 
    --mapM_ print $ [apply (deMorganDerivative1Strategy) $ newContext $ termNavigator x | x <- samples] 
    --mapM_ print $ [apply (evalStrategy doubleNot) $ newContext $ termNavigator x | x <- samples]
    --mapM_ print $ map (applyAll ruleDeMorgan) samples    
    --mapM_ print $ map (applyAll deMorgan) samples 
    --putStrLn "\n"
    --mapM_ print $ [apply (evalStrategy deMorgan) $ newContext $ termNavigator x | x <- samples]

    putStrLn "\nApply evalStrategy: Create a strategy to apply a rule or rules multiple times - Standard"
    print $ [apply (evalStrategy doubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy deMorgan) $ newContext $ termNavigator x | x <- samples]    
    print $ [apply (evalStrategy doubleNotOrDeMorgan) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy deMorganAndNextDoubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy deMorganInterleaveDoubleNot) $ newContext $ termNavigator x | x <- samples]    

    putStrLn "\nApply evalStrategy: Create a strategy to apply a rule or rules multiple times - Maybe"
    print $ [apply (evalStrategy2 doubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy2 deMorgan) $ newContext $ termNavigator x | x <- samples]    
    print $ [apply (evalStrategy2 doubleNotOrDeMorgan) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy2 deMorganAndNextDoubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy2 deMorganInterleaveDoubleNot) $ newContext $ termNavigator x | x <- samples]    

    putStrLn "\nApply evalStrategy: Single application before multiple application"
    print $ [apply (evalStrategy3 doubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy3 deMorgan) $ newContext $ termNavigator x | x <- samples]    
    print $ [apply (evalStrategy3 doubleNotOrDeMorgan) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy3 deMorganAndNextDoubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy3 deMorganInterleaveDoubleNot) $ newContext $ termNavigator x | x <- samples]    

    putStrLn "\nApply evalStrategy: Single application before multiple application (Left-biased choice: if the left-operand strategy can be applied)"
    print $ [apply (evalStrategy4 doubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy4 deMorgan) $ newContext $ termNavigator x | x <- samples]    
    print $ [apply (evalStrategy4 doubleNotOrDeMorgan) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy4 deMorganAndNextDoubleNot) $ newContext $ termNavigator x | x <- samples]
    print $ [apply (evalStrategy4 deMorganInterleaveDoubleNot) $ newContext $ termNavigator x | x <- samples]    

    putStrLn "\nApply evalStrategy: Single application before multiple application (Left-biased choice: if the left-operand strategy can be applied) - rule based"
    --print $ [apply (evalStrategy4 (singleRuleStartegy doubleNotRule)) $ newContext $ termNavigator x | x <- samples]    

    putStrLn "\nApply evalStrategy: Single application one rule before multiple application second rule"
    print $ [apply (evalStrategy5 doubleNot deMorgan) $ newContext $ termNavigator x | x <- deMorganTestSet]    
    --print $ [apply (evalStrategy5 (singleRuleStartegy doubleNotRule) (singleRuleStartegy deMorganRule)) $ newContext $ termNavigator x | x <- deMorganTestSet]    


    putStrLn "\nApply evalStrategy: Single application of one rule somewhere"
    --print $ applyAll (evalStrategy6 (singleRuleStartegy doubleNotRule)) $ newContext $ termNavigator (samples !! 2)

    print $ apply (multipleRuleChoiceStartegy [ruleDoubleNot, ruleDeMorgan]) $ newContext $ termNavigator (samples !! 7)
    --print $ [apply (multipleRuleChoiceStartegy [doubleNotRule, deMorganRule]) $ newContext $ termNavigator x | x <- samples]    

    --putStrLn "\nPrint Derivation minimalExercise:"
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (head samples)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 1)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 2)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 3)    
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 4)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 5)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (samples !! 6)
    --printDerivation (minimalExercise doubleNotOrDeMorgan) (last samples)

    --putStrLn  "\nPrint Derivation basicExercise:"
    --printDerivation (basicExercise doubleNotOrDeMorgan) (head samples)    
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 1)
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 2)
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 3)    
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 4)
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 5)
    --printDerivation (basicExercise doubleNotOrDeMorgan) (samples !! 6)    
    --printDerivation (basicExercise doubleNotOrDeMorgan) (last samples)        

    --putStrLn  "\nPrintDerivation basicExercise:"
    --printDerivation (evalExercise doubleNotOrDeMorgan) (head samples)
    --printDerivations (evalExercise doubleNotOrDeMorgan) (head samples)
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 1)    
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 2)
    --printDerivations (evalExercise doubleNotOrDeMorgan) (samples !! 2)
    --print $ showDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 2)
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 3)
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 4)
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 5)
    --printDerivation (evalExercise doubleNotOrDeMorgan) (samples !! 6)    
    --printDerivation (evalExercise doubleNotOrDeMorgan) (last samples)
--}
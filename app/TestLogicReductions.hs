module TestLogicReductions (main) where

import Ideas.Common.Library
import Ideas.Main.Default
import Domain.Logic.Formula 
import LogicReductionRules
import LogicReductionStrategies
import LogicExercices
import LogicTestCases
import LogicTestFunctions
import LogicTests

main :: IO ()
main = do
    --tstRuleDeMorganOrSimple
    --tstRuleDeMorganOrComplex
    --tstRuleDeMorganAndSimple
    --tstRuleDeMorganAndComplex
    --tstStratDeMorganSimple
    --tstStratDeMorganComplex


    --pptest "deMorganDerivTestSet" deMorganDerivTestSet
    --pptest "DeMorgan Strategy" [applyD (deMorgan) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 
    --pptest "Test Layer Top All" [applyD (testlta ruleDeMorganAnd) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    --mapM_ print $ map (apply ruleCommutativityOrdered) commutativityTestSet
    --mapM_ print $ map (apply ruleCommutativity) commutativityTestSet 
    -- putStrLn "\n"    
    --mapM_ print $ map (apply ruleCommutativityOrdered) commutativityTestSet 
    --mapM_ print $ [apply (checkStrategy) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet] 
    --mapM_ print $ [apply (layerFirst  lift ruleCommutativity) $ newContext $ termNavigator x | x <- implicationEliminationDerivTestSet]     
    --mapM_ print $ [apply (testlf ruleDeMorganAnd) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    -- putStrLn "\n"    
    --mapM_ print $ [apply (testlf2 ruleDeMorganOr) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    -- putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleChoiceStrategy [ruleDeMorganAnd, ruleDeMorganOr])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    -- putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleOrElseStrategy [ruleDeMorganAnd, ruleDeMorganOr])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 

    -- putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleChoiceStrategy [ruleDeMorganAnd, ruleDeMorganOr, ruleTRuleNotF])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    --putStrLn "\n"         
    --mapM_ print $ [apply (testlf3 (multiRuleChoiceStrategy [ruleDeMorganAnd, ruleDeMorganOr, ruleFRuleNotT, ruleTRuleNotF])) $ newContext $ termNavigator x | x <- deMorganDerivTestSet] 


    --putStrLn "\nruleCommutativity:"
    --mapM_ print $ map (applyD ruleCommutativity) commutativityTestSet    

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
--}
{-- Test code:   
    putStrLn "hasRule / applyRule / applyRuleD doubleNotRule:"
    print $ map (hasRule doubleNotRule) samples
    print $ map (applyRule doubleNotRule) samples
    print $ map (applyRuleD doubleNotRule) samples

    putStrLn "\napplyRuleAll doubleNotRule:"
    --print $ applyRuleAll reduction doubleNot (samples !! 1)
    print $ applyRuleAll doubleNotRule (samples !! 2)
    print $ applyRuleAll doubleNotRule (samples !! 3)    
    print $ applyRuleAll doubleNotRule (samples !! 7)
    print $ applyRuleAll doubleNotRule (samples !! 8)    

    putStrLn "\nhasRule / applyRule / applyRuleD deMorganRule:"
    print $ map (hasRule deMorganRule) samples
    print $ map (applyRule deMorganRule) samples
    print $ map (applyRuleD deMorganRule) samples

    putStrLn "\napplyRuleAll deMorganRule:"    
    print $ applyRuleAll doubleNotRule (samples !! 2)
    print $ applyRuleAll doubleNotRule (samples !! 3)
    print $ applyRuleAll doubleNotRule (samples !! 7)
    print $ applyRuleAll doubleNotRule (samples !! 8)    

    putStrLn "\nApply doubleNotOrDeMorgan:"
    print $ map (apply doubleNotOrDeMorgan) samples
    print $ map (applyD doubleNotOrDeMorgan) samples
    print $ map (applyAll doubleNotOrDeMorgan) samples

    putStrLn "\nApply minimalExercise:"
    print $ map (apply (minimalExercise doubleNotOrDeMorgan)) samples
    print $ map (applyD (minimalExercise doubleNotOrDeMorgan)) samples
    print $ map (applyAll (minimalExercise doubleNotOrDeMorgan)) samples    
    --putStrLn "\n"
    --mapM_ print $ map (apply ruleDoubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (apply doubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (applyD ruleDoubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (applyD doubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (applicable ruleDoubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (applicable doubleNot) samples    
    --putStrLn "\n"
    --mapM_ print $ map (applyAll ruleDoubleNot) samples    
    --putStrLn "\ncommutativity:"
    --mapM_ print $ map (applyAll ruleCommutativity) samples    

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
module LogicTests where

import Domain.Logic.Formula
import Ideas.Common.Library
import LogicReductionRules
import LogicReductionStrategies
import LogicTestCases
import LogicTestFunctions

quickTestSet :: LsLgcChar
quickTestSet = [
                Not (Var 'p' :&&: Var 'q'),                                         -- ¬(p ˄ q)
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r'),                            -- ¬(p ˄ q ˄ r)
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's'),               -- ¬(p ˄ q ˄ r ˄ s)
                Not (Var 'q' :&&: Var 'r' :&&: Var 's' :&&: Var 'p'),               -- ¬(p ˄ r ˄ s ˄ p)                
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's' :&&: Var 't'),  -- ¬(p ˄ q ˄ r ˄ s ˄ t)
                Not (Not (Var 'p' :&&: Var 'q')),                                   -- ¬¬(p ˄ q)
                Not (Not (Var 'q' :&&: Var 'p')),                                   -- ¬¬(q ˄ p)  
                Not (Not (Var 'q' :&&: Var 'r' :&&: Var 'p')),                      -- ¬¬(q ˄ r ˄ p)                
                Not (Not (Not (Var 'p')) :&&: T),                                   -- ¬(¬¬p ˄ T) 
                Not (Not (Not (Var 'p')) :&&: T :&&: T),                            -- ¬(¬¬p ˄ T ˄ T)
                Not (Not (Not (Var 'p')) :&&: T :&&: F),                            -- ¬(¬¬p ˄ T ˄ F)                
                Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'p'))),          -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'q'))),          -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                Not (Not (Not (Var 'p')) :&&: Not (Var 'p') :&&: T)                 -- ¬(¬¬p ˄ ¬p ˄ T)
               ]

tstRuleDeMorganOrSimple, tstRuleDeMorganOrComplex, tstRuleDeMorganAndSimple, tstRuleDeMorganAndComplex, tstRuleDeMorganSimple, tstRuleDeMorganComplex, 
    tstRuleCommutativity :: IO ()

tstRuleDeMorganOrSimple = do
    putStrLn "DeMorgan-Or Rule"
    let xs = deMorganOrTestSetSimple
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganOr) $ newContext $ termNavigator x | x <- xs ]

tstRuleDeMorganOrComplex = do
    putStrLn "DeMorgan-Or Rule"
    let xs = deMorganOrTestSetComplex
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganOr) $ newContext $ termNavigator x | x <- xs ]

tstRuleDeMorganAndSimple = do
    putStrLn "DeMorgan-And Rule"
    let xs = deMorganAndTestSetSimple
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- xs ]

tstRuleDeMorganAndComplex = do
    putStrLn "DeMorgan-And Rule"
    let xs = deMorganAndTestSetComplex
    pptest "Input  (Complex testset): " xs
    pptest "Output (Complex testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- xs ]

testRuleDeMorganAndComplex = do
    putStrLn "DeMorgan-And Rule"
    let xs = deMorganAndTestSetComplex
    pptest "Input  (Complex testset): " xs
    pptest "Output (Complex testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- xs ]

tstRuleDeMorganSimple = do
    putStrLn "DeMorgan-And Rule"
    let xs = deMorganAndTestSetSimple ++ deMorganOrTestSetSimple
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorgan) $ newContext $ termNavigator x | x <- xs ]

tstRuleDeMorganComplex = do
    putStrLn "DeMorgan-And Rule"
    let xs = deMorganAndTestSetComplex ++ deMorganOrTestSetComplex
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorgan) $ newContext $ termNavigator x | x <- xs ]

tstRuleCommutativity = do
    putStrLn "Commutativity"
    let xs = commutativityTestSet
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleCommutativity) $ newContext $ termNavigator x | x <- xs ]

tstRuleCommutativityOrd = do
    putStrLn "Ordered Commutativity"
    let xs = commutativityTestSet
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext ruleCommutativityOrd) $ newContext $ termNavigator x | x <- xs ]

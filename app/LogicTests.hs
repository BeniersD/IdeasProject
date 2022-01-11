module LogicTests where

import Domain.Logic.Formula
import Ideas.Common.Library
import LogicReductionRules
import LogicReductionStrategies
import LogicTestCases
import LogicTestFunctions

quickTestSet :: LsLgcChar
quickTestSet = [
                Not (Var 'p' :&&: Var 'q'),
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r'),
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's'), 
                Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's' :&&: Var 't'),
                Not (Not (Var 'p' :&&: Var 'q')), 
                Not (Not (Var 'q' :&&: Var 'p' :&&: Var 'r')),
                Not (Not (Var 'q' :&&: Var 'p')),                             -- ¬¬(q ˄ p)  
                Not (Not (Not (Var 'p')) :&&: T),                             -- ¬(¬¬p ˄ T) 
                Not (Not (Not (Var 'p')) :&&: T :&&: T),                      -- ¬(¬¬p ˄ T ˄ T)
                Not (Not (Not (Var 'p')) :&&: T :&&: F),                      -- ¬(¬¬p ˄ T ˄ F)                
                Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'p'))),    -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'q'))),    -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                Not (Not (Not (Var 'p')) :&&: Not (Var 'p') :&&: T)          -- ¬(¬¬p ˄ ¬p ˄ T)
               ]

tstRuleDeMorganOrSimple, tstRuleDeMorganOrComplex, tstRuleDeMorganAndSimple, tstRuleDeMorganAndComplex, testRuleDeMorganAndComplex, 
    tstStratDeMorganSimple :: IO ()
tstRuleDeMorganOrSimple = do
    putStrLn "DeMorgan-Or Rule"
    let testset = deMorganOrTestSetSimple
    pptest "Input  (Simple testset): " testset
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganOr) $ newContext $ termNavigator x | x <- testset ]

tstRuleDeMorganOrComplex = do
    putStrLn "DeMorgan-Or Rule"
    let testset = deMorganOrTestSetComplex
    pptest "Input  (Simple testset): " testset
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganOr) $ newContext $ termNavigator x | x <- testset ]

tstRuleDeMorganAndSimple = do
    putStrLn "DeMorgan-And Rule"
    let testset = deMorganAndTestSetSimple
    pptest "Input  (Simple testset): " testset
    pptest "Output (Simple testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- testset ]

tstRuleDeMorganAndComplex = do
    putStrLn "DeMorgan-And Rule"
    let testset = deMorganAndTestSetComplex
    pptest "Input  (Complex testset): " testset
    pptest "Output (Complex testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- testset ]

testRuleDeMorganAndComplex = do
    putStrLn "DeMorgan-And Rule"
    let testset = deMorganAndTestSetComplex
    pptest "Input  (Complex testset): " testset
    pptest "Output (Complex testset): " [applyD (liftToContext ruleDeMorganAnd) $ newContext $ termNavigator x | x <- testset ]

tstStratDeMorganSimple = do
    putStrLn "DeMorgan-And Rule"
    let testset = deMorganAndTestSetSimple ++ deMorganOrTestSetSimple
    pptest "Input  (Simple testset): " testset
    pptest "Output (Simple testset): " [applyD (stratDeMorgan) $ newContext $ termNavigator x | x <- testset ]

tstStratDeMorganComplex = do
    putStrLn "DeMorgan-And Rule"
    let testset = deMorganAndTestSetComplex ++ deMorganOrTestSetComplex
    pptest "Input  (Simple testset): " testset
    pptest "Output (Simple testset): " [applyD (stratDeMorgan) $ newContext $ termNavigator x | x <- testset ]

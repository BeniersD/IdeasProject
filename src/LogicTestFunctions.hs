{-# LANGUAGE TypeFamilies #-}

module LogicTestFunctions (tstRuleGeneric, tstStrategyGeneric, clean, tstDerivation, tstApply)
    where

import Data.Foldable as Foldable
import Domain.Logic.Formula
import Ideas.Common.Library
import LogicExercices
import LogicReductionStrategies

tupleToStr :: (Show a) => (Int, a) -> String
tupleToStr x = numbers ++ ". " ++ formula ++ "\n"
    where
        numbers = show $ fst x
        formula = show $ snd x

resultToStr :: (Show a) => [(Int, a)] -> String
resultToStr = foldl' (++) "" . map tupleToStr

zipResults :: [a] -> [(Int, a)]
zipResults xs = zip numbers xs
    where
        len = length xs
        numbers = [0..len-1]

pptest :: (Show a) => String -> [a] -> IO ()
pptest desc xs = putStr $ desc ++ ":\n" ++ result ++ "\n"
    where 
        resultset  = zipResults xs
        result     = resultToStr resultset

clean :: String -> String
clean []        = []
clean ('\n':[]) = []
clean ('\n':' ':' ':xs) = clean xs
clean ('\n':xs) = ' ':'=':'>':' ':clean xs
clean (x:xs)    = x:clean xs


-- Shows result after application of a rule 
tstRuleGeneric:: (HasId (t a), Show a, Apply t) => t a -> [a] -> IO ()
tstRuleGeneric  r xs = do
    let desc = "Rule: " ++ description r ++ "\n"
    putStrLn desc
    pptest "Input  (Simple testset): " xs
    -- Show result if applicable
    --pptest "Output (Simple testset): " [applicable r x | x <- xs ]

    -- Show result if applicable - Just / Nothing
    --pptest "Output (Simple testset): " [apply r  x | x <- xs ]

    -- Show result after rewrite
    pptest "Output (Simple testset): " [applyD r x | x <- xs ]


-- Shows result after application of a strategy 
tstStrategyGeneric :: (HasId (t (Context a)), Show a, Apply t, IsTerm a) => t (Context a) -> [a] -> IO ()
tstStrategyGeneric  s xs = do
    let desc = "Rule: " ++ description s ++ "\n"
    putStrLn desc
    pptest "Input  (Simple testset): " xs
    -- Show result if applicable
    --pptest "Output (Simple testset): " [applicable s $ newContext $ termNavigator x | x <- xs ]

    -- Show result if applicable - Just / Nothing
    -- pptest "Output (Simple testset): " [apply s $ newContext $ termNavigator x | x <- xs ]

    -- Show result after rewrite
    pptest "Output (Simple testset): " [applyD s $ newContext $ termNavigator x | x <- xs ]


-- Shows the (all) derivation(s) of the strategy or rule    
tstDerivation :: (LogicEvaluationStrategy a, Out2 a ~ (t -> LabeledStrategy (Context SLogic))) => a -> t -> [SLogic] -> IO ()
--tstDerivation r s xs = mapM_ (\(x, y) -> putStrLn $  show x ++ ". " ++ clean y) [(y, show x ++ " :\t" ++ (f x)) | (x, y) <- zip (t xs) [0..]]
tstDerivation r s xs = mapM_ (\(x, y) -> putStrLn $  show x ++ ". " ++ y) [(y, show x ++ " :\t" ++ (f x)) | (x, y) <- zip (t) [0..]]
    where
        --t  = (take 100) xs
        t  = (drop 0 . take 1000) xs
        es = evalStrategy r s
        --ex = minimalExercise es
        --ex = basicExercise es
        ex = evalExercise es
        f  = showDerivation ex
        --f  = showDerivations ex


-- Delivers a list with the results after application of the strategy or rule
tstApply r s xs = print $ map a t 
    where 
        --t  = (take 100) xs
        t  = (drop 0 . take 1000) xs
        es = evalStrategy r s
        --ex = minimalExercise es
        ex = basicExercise es
        --ex = evalExercise es
        --a  = applicable ex              
        --a  = apply ex
        a  = applyD ex
        --a  = applyAll ex
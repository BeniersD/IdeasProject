{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicTestFunctions 
    where

import Data.Function
import Data.Typeable
import Data.Foldable as Foldable
import Domain.Logic.Formula  hiding (not)
import Ideas.Common.Library
import LogicReductionStrategies
import LogicTestCases

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Generic functions for test purposes
-------------------------------------------------------------------------------------------------------------------------------------------------
data EvaluationType = Single | SomeWhere | SomeWhereRepeatS | SomeWhereRepeat1 | RepeatS | Repeat1 

class LogicRuleConversion a where
    type Out a     :: * 
    ruleToStrategy :: a -> Out a

instance LogicRuleConversion (Rule SLogic) where  
    type Out (Rule SLogic) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = ruleToStrategy (liftToContext x)

instance LogicRuleConversion (Rule (Context SLogic)) where 
    type Out (Rule (Context SLogic)) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = label (showId x) $ x

class LogicEvaluationStrategy a where
    type Out2 a  :: * 
    evalStrategy :: a -> Out2 a
    
instance LogicEvaluationStrategy (Rule SLogic) where  
    type Out2 (Rule SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (ruleToStrategy r) 

instance LogicEvaluationStrategy (Rule (Context SLogic)) where  
    type Out2 (Rule (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (ruleToStrategy r)
    
instance LogicEvaluationStrategy (LabeledStrategy SLogic) where  
    type Out2 (LabeledStrategy SLogic) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r      = evalStrategy (liftToContext r)

instance LogicEvaluationStrategy (LabeledStrategy (Context SLogic)) where  
    type Out2 (LabeledStrategy (Context SLogic)) = (EvaluationType -> LabeledStrategy (Context SLogic))
    evalStrategy r Single           = evalStrategyG ("Evaluate - " ++ showId r)                     r 
    evalStrategy r SomeWhere        = evalStrategyG ("Evaluate somewhere - " ++ showId r)           (somewhere r)
    evalStrategy r RepeatS          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeatS   r) 
    evalStrategy r Repeat1          = evalStrategyG ("Evaluate repeat - " ++ showId r)              (repeat1   r) 
    evalStrategy r SomeWhereRepeatS = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeatS   (somewhere r))
    evalStrategy r SomeWhereRepeat1 = evalStrategyG ("Evaluate repeat somewhere - " ++ showId r)    (repeat1   (somewhere r))

evalStrategyG :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
evalStrategyG l s       = label l $ s

minimalExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
minimalExercise x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (minimal)" $ newId "eval.minimal", 
                        strategy      = x, 
                        prettyPrinter = show
                     }

basicExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
basicExercise   x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (basic)" $ newId "eval.basic",
                        strategy      = x, 
                        navigation    = termNavigator, 
                        prettyPrinter = show
                     }

evalExercise :: LabeledStrategy (Context SLogic) -> Exercise SLogic
evalExercise    x = emptyExercise
                     { 
                        exerciseId    = describe "Evaluate an expression (full)" $ newId "eval.full", 
                        status        = Experimental, 
                        strategy      = x, 
                        prettyPrinter = show, 
                        navigation    = termNavigator, 
                        --parser        = readM, 
                        equivalence   = withoutContext eqExpr,
                        -- properties    = setProperty "indistinguishability" withoutContext (indExpr x),
                        -- ->? -
                        similarity    = withoutContext (==), 
                        ready         = predicate (const True),-- isTerm, 
                        examples      = examplesFor Easy commutativityTestSet
                     }

execStrategy :: LabeledStrategy (Context SLogic) -> SLogic -> String
execStrategy s x = show $ applyD s (newContext $ termNavigator x)

eqExpr :: SLogic -> SLogic -> Bool
eqExpr x y = (execStrategy stratAC x) == (execStrategy stratAC y)


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
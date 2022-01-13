module LogicTestFunctions where

import Data.Foldable as Foldable
import Domain.Logic.Formula
import Ideas.Common.Library
import Data.Function
import LogicTestCases
import LogicReductionRules
import Ideas.Utils.Prelude

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
        numbers = [1..length xs]

pptest :: (Show a) => String -> [a] -> IO ()
pptest desc xs = putStr $ desc ++ ":\n" ++ result ++ "\n"
    where 
        resultset  = zipResults xs
        result     = resultToStr resultset

tstRuleGeneric :: (Show a, IsTerm a) => Rule (Logic a) -> [Logic a] -> IO ()
tstRuleGeneric r xs = do
    let desc = "Rule: " ++ description r ++ "\n"
    putStrLn desc
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext r) $ newContext $ termNavigator x | x <- xs ]

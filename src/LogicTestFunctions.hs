module LogicTestFunctions (tstRuleGeneric)
    where

import Data.Foldable as Foldable
import Domain.Logic.Formula
import Ideas.Common.Library
import LogicFunctions

--import Data.Function
--import LogicTestCases
--import LogicReductionRules
--import Ideas.Utils.Prelude

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

tstRuleGeneric :: Rule (SLogic) -> [SLogic] -> IO ()
tstRuleGeneric r xs = do
    let desc = "Rule: " ++ description r ++ "\n"
    putStrLn desc
    pptest "Input  (Simple testset): " xs
    pptest "Output (Simple testset): " [applyD (liftToContext r) $ newContext $ termNavigator x | x <- xs ]


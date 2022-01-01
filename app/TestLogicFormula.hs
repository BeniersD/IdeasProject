module TestLogicFormula where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Foldable (toList)
import Data.List
import Data.Monoid
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Typeable
import Domain.Algebra.Boolean
import Domain.Logic.Formula
import Domain.Logic.Views
import LogicReductionRules_Old
import Ideas.Main.Default

main = do
    putStrLn  "ppLogic:"
    print $ ppLogic (Var "p" :<->: Var "q")
    print $ ppLogic (Var "p" :<->: Not(Var "q"))
    putStrLn  "\nshow:"
    print $ show (Var "p" :<->: Var "q")
    putStrLn  "\nisAtomic:"
    print $ isAtomic T
    putStrLn  "\nisDNF:"
    print $ isDNF (Var "p" :&&: Var "q")
    print $ isDNF (Not(Var "p") :->: Var "q")
    putStrLn  "\neqLogic:"
    print $ eqLogic (Var "p") (Var "q")
    print $ eqLogic (Var "p") (Var "p")
    print $ eqLogic (Not(Var "p")) (Var "p")
    putStrLn  "\ntautology:"
    print $ tautology (Var "p" :->: Var "q")
    print $ tautology (Var "p" :||: Not(Var "p")) 
    print $ tautology ((Var "p" :->: Var "q") :<->: (Not(Var "p") :||: Var "q")) 
    putStrLn  "\nvarslogic:"
    print $ varsLogic ((Var "p" :->: Var "q") :<->: (Not(Var "p") :||: Var "q")) 
    putStrLn  "\ncountEquivalences:"
    print $ countEquivalences ((Var "p" :->: Var "q") :<->: (Not(Var "p") :<->: Var "q"))
    putStrLn  "\nisNot:"
    print $ isNot ( Not F )
    print $ isNot ( Var "p" )
    print $ isNot ((Var "p" :->: Var "q") :<->: (Not(Var "p") :<->: Var "q"))
    putStrLn  "\npushNot:"
    print $ pushNot (Var "p" .<->. Var "q")
    print $ pushNot (Not(Var "p") .<->. Var "q")
    putStrLn  "\nSimplify:" 
    print $ simplify (Var "p" .<->. Var "q")
    print $ simplify (Var "p" .<->. T)
    print $ simplify (Not(Var "p") .<->. T)
    putStrLn  "pushNotWith:"
    print $ pushNot (Not(Var "p") .<->. Var "q")
    print $ pushNot ((Var "p" :->: Var "q") :<->: (Not(Var "p") :||: Var "q"))
    putStrLn  "PrettyPrint Logic:"
    print $ ppLogic ((Var "p" :->: Var "q") :<->: (Not(Var "p") :||: Var "q")) 
    putStrLn  "\nisDoubleNot:"
    print $ isDoubleNot (Var "p")
    print $ isDoubleNot (Not(Var "p"))
    print $ isDoubleNot (Not(Not(Var "p")))
    print $ isDoubleNot ((Var "p" :->: Not(Not(Var "p"))) :<->: (Not(Var "p") :<->: Not(Var "p")))
    putStrLn  "\nhasDoubleNot:"
    print $ hasDoubleNot (Var "p")
    print $ hasDoubleNot (Not(Var "p"))
    print $ hasDoubleNot (Not(Not(Var "p")))
    print $ hasDoubleNot ((Var "p" :->: Not(Not(Var "p"))) :<->: (Not(Var "p") :<->: Not(Var "p")))
    print $ hasDoubleNot ((Var "p" :->: Not(Not(Var "p"):<->:Not(Not(Var "p")))):<->:(Not(Var "p"):<->:Not(Var "p")))
    print $ singleDoubleNot   (Not (Not (Var 'p'))) :&&: Not( Not( Var 'p'))
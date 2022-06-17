import Domain.Logic.Formula hiding (SLogic)
import LogicReductionRules
import Ideas.Common.Rewriting.Unification
import Ideas.Common.Rewriting.Term

--------------------------------------------------------------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------------------------------------------------------------
o = Var "o"
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"

--------------------------------------------------------------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------------------------------------------------------------
logic1 :: SLogic 
logic1 = p :&&: q

logic2 :: SLogic 
logic2 = Not (Not p)

logic3 :: SLogic 
logic3 = Not p

logic4 :: SLogic 
logic4 = Not (p :&&: q)

term1 :: Term 
term1 = toTerm logic1

term2 :: Term 
term2 = toTerm logic2

term3 :: Term 
term3 = toTerm logic3

term4 :: Term 
term4 = toTerm logic4

--------------------------------------------------------------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  print logic1
  print term1
  print logic2
  print term2

  print $ unify term1 term1
  print $ unify term1 term2
  print $ unify term1 term3

  print $ unify term2 term1
  print $ unify term2 term2
  print $ unify term2 term3

  print $ unify term3 term1
  print $ unify term3 term2
  print $ unify term3 term3

  print $ unify term3 term4
  print $ unify term4 term3

  


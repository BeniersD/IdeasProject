module LogicFunctions (isOrdered, isMultiDoubleNot, isUnaryTerm
   ) where

import Domain.Logic.Formula

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic checker functions
--------------------------------------------------------------------------------------------------------------------------------------
isOrdered, isMultiDoubleNot, isUnaryTerm :: SLogic -> Bool
isOrdered (p :||: q) | p >= q = True
isOrdered (p :&&: q) | p >= q = True
isOrdered _                   = False

isMultiDoubleNot (Not (Not (Not (Not p)))) = True
isMultiDoubleNot _                         = False

isUnaryTerm (Not p) = isUnaryTerm p
isUnaryTerm p = isAtomic p
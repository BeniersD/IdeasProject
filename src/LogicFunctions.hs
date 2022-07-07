module LogicFunctions (isWff, isOrdered
   ) where

import Domain.Logic.Formula

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic checker functions
--------------------------------------------------------------------------------------------------------------------------------------
isWff :: Logic a -> Bool
isWff l = case l of
         T             -> True
         F             -> True
         ( Var _ )     -> True
         ( Not x )     -> isWff x
         ( x :&&: y )  -> isWff x && isWff y
         ( x :||: y )  -> isWff x && isWff y
         ( x :<->: y ) -> isWff x && isWff y
         ( x :->: y)   -> isWff x && isWff y
--         _             -> False


isOrdered (p :||: q) | p >= q = True
isOrdered (p :&&: q) | p >= q = True
isOrdered _                   = False
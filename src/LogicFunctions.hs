module LogicFunctions (isWff)
    where
import Domain.Logic.Formula

isWff :: SLogic -> Bool
isWff T             = True
isWff F             = True
isWff ( Var _ )     = True
isWff ( Not x )     = isWff x
isWff ( x :->:  y ) = isWff x && isWff y
isWff ( x :<->: y ) = isWff x && isWff y
isWff ( x :&&:  y ) = isWff x && isWff y
isWff ( x :||:  y ) = isWff x && isWff y
module LogicFunctions (isOrdered, isDoubleNot, isMultiDoubleNot, isUnaryTerm, isAssoCommOrdered, skipNegations, countNegations, compareLogic, skipNegation, isNegation, isThruthValue
   ) where

import Domain.Logic.Formula
import Data.Maybe

--------------------------------------------------------------------------------------------------------------------------------------
-- Generic checker functions
--------------------------------------------------------------------------------------------------------------------------------------
isAssoCommOrdered, isDoubleNot, isMultiDoubleNot, isNegation, isOrdered, isThruthValue, isUnaryTerm  :: SLogic -> Bool

isAssoCommOrdered (p :||: q) | (skipNegations p /= skipNegations q) = isOrdered (skipNegations p :||: skipNegations q)
isAssoCommOrdered (p :&&: q) | (skipNegations p /= skipNegations q) = isOrdered (skipNegations p :&&: skipNegations q)
isAssoCommOrdered x                                                 = isOrdered x

isDoubleNot (Not (Not p))                  = True
isDoubleNot _                              = False

isMultiDoubleNot (Not (Not (Not (Not p)))) = True
isMultiDoubleNot _                         = False

isNegation (Not _)                         = True
isNegation _                               = False

isOrdered (p :||: q) | (isThruthValue . skipNegations) p && (not . isUnaryTerm) q = True
isOrdered (p :&&: q) | (isThruthValue . skipNegations) p && (not . isUnaryTerm) q = True

isOrdered (p :||: q) | (not . isUnaryTerm) p && (isThruthValue . skipNegations) q = False
isOrdered (p :&&: q) | (not . isUnaryTerm) p && (isThruthValue . skipNegations) q = False

isOrdered (p :||: q) | (isThruthValue p && isThruthValue q) = compareLogic p q 
isOrdered (p :&&: q) | (isThruthValue p && isThruthValue q) = compareLogic p q 

isOrdered (p :||: q) | p <= q                                                      = True
isOrdered (p :&&: q) | p <= q                                                      = True

isOrdered _                                                                        = False

isThruthValue x = (isTrue . skipNegations) x || (isFalse . skipNegations) x

isUnaryTerm (Not p)                        = isUnaryTerm p
isUnaryTerm p                              = isAtomic p

compareLogic :: SLogic -> SLogic -> Bool 
compareLogic p q | skipNegations p == skipNegations q           = countNegations p <= countNegations q
                 | skipNegations p == T && skipNegations q == F = False
                 | skipNegations p == F && skipNegations q == T = True
                 | otherwise                                    = skipNegations p  <= skipNegations q 

skipNegation, skipNegations :: SLogic -> SLogic
skipNegation  x = fromMaybe x (isComplement x) 

skipNegations x | isNegation x = skipNegations(skipNegation x)
                | otherwise    = x


countNegations :: Num p => Logic a -> p
countNegations (Not x) = 1 + countNegations x
countNegations _       = 0


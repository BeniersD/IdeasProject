module LogicFunctions ( createRule, convertToRule, countNegations, compareLogic, hasRule, hasBool, isAndOrOr, isAssoCommOrdered, isBool,
   isMultiAnd, isDoubleNot, isMultiAndOr,isMultiDoubleNot, isMultiImplicationDefinition, isMultiOr, isNegation, isOrdered,  isUnary, 
   skipNegation, skipNegations 
   ) where

import Data.Maybe
import Domain.Logic.Formula
import Ideas.Common.Library hiding (isUnary)
import Ideas.Utils.Uniplate

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Generic Logic functions
-------------------------------------------------------------------------------------------------------------------------------------------------
createRule     :: String -> String -> (SLogic -> Maybe (SLogic)) -> Rule SLogic
createRule x y f                                                            = describe ( "Rewrite " ++ x ) $ makeRule y f

convertToRule  :: Eq a => String -> String -> LabeledStrategy a -> Rule a
convertToRule x y f                                                         = describe ( "Rewrite " ++ x ) $ makeRule y (apply f)

countNegations :: Num p => Logic a -> p
countNegations (Not x)                                                      = 1 + countNegations x
countNegations _                                                            = 0

compareLogic   :: SLogic -> SLogic -> Bool 
compareLogic p q | skipNegations p == skipNegations q                       = countNegations p <= countNegations q
                 | skipNegations p == T && skipNegations q == F             = False
                 | skipNegations p == F && skipNegations q == T             = True
                 | otherwise                                                = skipNegations p  <= skipNegations q 

hasDoubleNot, hasBool :: SLogic -> Bool
hasDoubleNot p                                                              = any isDoubleNot (children p)

hasBool p                                                                   = isBool p || any isBool (children p)

hasRule      :: Rule SLogic -> SLogic -> Bool
hasRule x                                                                   = isJust . apply x

isAndOrOr, isAssoCommOrdered, isDoubleNot, isMultiImplicationDefinition, isMultiAnd, isMultiAndOr, isMultiDoubleNot, isMultiOr, 
   isNegation, isOrdered, isBool, isUnary :: SLogic -> Bool
isAndOrOr x                                                                 = isJust (isOr x) || isJust (isAnd x)

isAssoCommOrdered (p :||: q) | (skipNegations p /= skipNegations q)         = isOrdered (skipNegations p :||: skipNegations q)
isAssoCommOrdered (p :&&: q) | (skipNegations p /= skipNegations q)         = isOrdered (skipNegations p :&&: skipNegations q)
isAssoCommOrdered p                                                         = isOrdered p

isDoubleNot (Not (Not _))                                                   = True
isDoubleNot _                                                               = False

isMultiImplicationDefinition ( _ :->: _ :->: _ )                            = True
isMultiImplicationDefinition _                                              = False

isMultiAnd (_ :&&: _ :&&: _)                                                = True
isMultiAnd _                                                                = False

isMultiDoubleNot (Not (Not p))                                              = isDoubleNot p
isMultiDoubleNot _                                                          = False

isMultiAndOr (_ :&&: _ :||: _)                                              = True
isMultiAndOr (_ :||: _ :&&: _)                                              = True
isMultiAndOr p                                                              = isMultiAnd p || isMultiOr p

isMultiOr (_ :||: _ :||: _)                                                 = True
isMultiOr _                                                                 = False

isNegation (Not _)                                                          = True
isNegation _                                                                = False

isOrdered (p :||: q) | (isBool . skipNegations) p && (not . isUnary) q      = True
isOrdered (p :&&: q) | (isBool . skipNegations) p && (not . isUnary) q      = True
isOrdered (p :||: q) | (not . isUnary) p && (isBool . skipNegations) q      = False
isOrdered (p :&&: q) | (not . isUnary) p && (isBool . skipNegations) q      = False
isOrdered (p :||: q) | (isBool p && isBool q)                               = compareLogic p q 
isOrdered (p :&&: q) | (isBool p && isBool q)                               = compareLogic p q 
isOrdered (p :||: q) | p <= q                                               = True
isOrdered (p :&&: q) | p <= q                                               = True
isOrdered _                                                                 = False

isBool p                                                                    = (isTrue . skipNegations) p || (isFalse . skipNegations) p

isUnary (Not p)                                                             = isUnary p
isUnary p                                                                   = isAtomic p

skipNegation, skipNegations :: SLogic -> SLogic
skipNegation  x                                                             = fromMaybe x (isComplement x) 
skipNegations x | isNegation x                                              = skipNegations(skipNegation x)
                | otherwise                                                 = x



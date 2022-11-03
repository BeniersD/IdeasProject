{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicFunctions ( o, p, q, r, s ,t, createRule, convertToRule, countNegations, compareLogic, hasRule, hasBool, hasNegation, isAndOrOr, 
   isAssoCommOrdered, isBool, isDistAnd, isDistOr, isMultiAnd, isDoubleNot, isMultiAndOr, isMultiDoubleNot, isMultiImplicationDefinition, 
   isMultiOr, isNegation, isOrdered, isLiteral, skipNegation, skipNegations, ruleToStrategy, removeDuplicates, hasElement, matchingElement
   ) where

import Data.Maybe
import Domain.Logic.Formula
import Ideas.Common.Library
import Ideas.Utils.Prelude
import Ideas.Utils.Uniplate

------------------------------------------------------------------------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------------------------------------------------------------------------
o, p, q, r, s, t :: Logic ShowString
o = Var (ShowString "o")
p = Var (ShowString "p") 
q = Var (ShowString "q") 
r = Var (ShowString "r")
s = Var (ShowString "s")
t = Var (ShowString "t")

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Generic Logic functions
-------------------------------------------------------------------------------------------------------------------------------------------------
class LogicRuleConversion a where
    type Out a     :: * 
    ruleToStrategy :: a -> Out a

instance LogicRuleConversion (Rule SLogic) where  
    type Out (Rule SLogic) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = ruleToStrategy (liftToContext x)

instance LogicRuleConversion (Rule (Context SLogic)) where 
    type Out (Rule (Context SLogic)) = (LabeledStrategy (Context SLogic))
    ruleToStrategy x    = label (showId x) $ x


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

hasElement :: Eq a => a -> [a] -> Bool
hasElement e []                 = False
hasElement e (x:xs) | e == x    = True
                    | otherwise = hasElement e xs

matchingElement :: Eq a => [a] -> [a] -> Bool
matchingElement [] _          = False
matchingElement _ []          = False
matchingElement (x:xs) (y:ys) = x == y || matchingElement xs ys

hasRule :: Rule (Context SLogic) -> SLogic -> Bool
hasRule x y                                                                 =  applicable (somewhere x) (newContext $ termNavigator y)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (filter (/=x) xs)

hasBool, hasDoubleNot, hasNegation, isAndOrOr, isAssoCommOrdered, isDistAnd, isDistOr, isDoubleNot, isMultiImplicationDefinition, isMultiAnd, 
   isMultiAndOr, isMultiDoubleNot, isMultiOr, isNegation, isOrdered, isBool, isLiteral :: SLogic -> Bool
hasBool p                                                                   = isBool p || any isBool (children p)

hasDoubleNot p                                                              = any isDoubleNot (children p)

hasNegation p                                                               = any isNegation (children p)

isAndOrOr x                                                                 = isJust (isOr x) || isJust (isAnd x)

isAssoCommOrdered (p :||: q) | (skipNegations p /= skipNegations q)         = isOrdered (skipNegations p :||: skipNegations q)
isAssoCommOrdered (p :&&: q) | (skipNegations p /= skipNegations q)         = isOrdered (skipNegations p :&&: skipNegations q)
isAssoCommOrdered p                                                         = isOrdered p

isDistAnd ( _ :||: ( _ :&&: _ ))                                            = True
isDistAnd (( _ :&&: _ ) :||: _ )                                            = True
isDistAnd _                                                                 = False

isDistOr ( _ :&&: ( _ :||: _ ))                                             = True
isDistOr (( _ :||: _ ) :&&: _ )                                             = True
isDistOr _                                                                  = False  

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

isOrdered (p :||: q) | (isBool . skipNegations) p && (not . isLiteral) q      = True
isOrdered (p :&&: q) | (isBool . skipNegations) p && (not . isLiteral) q      = True
isOrdered (p :||: q) | (not . isLiteral) p && (isBool . skipNegations) q      = False
isOrdered (p :&&: q) | (not . isLiteral) p && (isBool . skipNegations) q      = False
isOrdered (p :||: q) | (isBool p && isBool q)                               = compareLogic p q 
isOrdered (p :&&: q) | (isBool p && isBool q)                               = compareLogic p q 
isOrdered (p :||: q) | p <= q                                               = True
isOrdered (p :&&: q) | p <= q                                               = True
isOrdered _                                                                 = False

isBool p                                                                    = (isTrue . skipNegations) p || (isFalse . skipNegations) p

isLiteral (Not p)                                                             = isLiteral p
isLiteral p                                                                   = isAtomic p

skipNegation, skipNegations :: SLogic -> SLogic
skipNegation  x                                                             = fromMaybe x (isComplement x) 
skipNegations x | isNegation x                                              = skipNegations(skipNegation x)
                | otherwise                                                 = x



module LogicReductionRules 
   ( LgcRule, LgcRed, LSLgc, LSCtxLgc, hasRule,  createRule, convertToRule,
     ruleDoubleNot, ruleDeMorganAnd, ruleDeMorganOr, ruleImplicationElimination, ruleEquivalenceElimination, ruleAbsorption, 
     ruleIdempotency, ruleFRuleConjunction, ruleTRuleConjunction, ruleFRuleDisjunction, ruleTRuleDisjunction, 
     ruleFRuleComplement, ruleTRuleComplement, ruleFRuleNotT, ruleTRuleNotF, ruleCommutativity, commutativity
   ) where

import Ideas.Common.Library hiding (description)
import Ideas.Main.Default
import Domain.Logic.Formula
import Ideas.Utils.Prelude
import Data.Maybe
import Data.List

type LgcRule a = Rule (Logic a)
type LgcRed a = Logic a -> Maybe (Logic a) 
type LSLgc a = LabeledStrategy (Logic a)
type LSCtxLgc a = LabeledStrategy (Context (Logic a))

------------------------------------------------------------------------------------------------------------
-- Generic rewrite/reduction functions
------------------------------------------------------------------------------------------------------------
hasRule :: LgcRule a -> Logic a -> Bool
hasRule x = isJust . apply x

createRule :: Ord a => Eq a => String -> String -> LgcRed a -> LgcRule a
createRule x y f = describe ( "Rewrite " ++ x ) $ makeRule ( "rewrite." ++ y ) f

convertToRule :: Eq a => String -> String -> LSLgc a -> LgcRule a
convertToRule x y f = describe ( "Rewrite " ++ x ) $ makeRule ( "rewrite." ++ y ) (apply f)

--convertLscToRule :: Eq a => String -> String -> LSCtxLgc a -> LgcRule a
--convertLscToRule x y f = describe ( "Rewrite " ++ x ) $ makeRule ( "rewrite." ++ y ) (apply (fromContext f))

------------------------------------------------------------------------------------------------------------
-- Set of rules
------------------------------------------------------------------------------------------------------------
ruleAbsorption, ruleAssociativity, ruleCommutativity, ruleDeMorganAnd, ruleDeMorganOr, ruleDoubleNot, ruleDistributivity, ruleEquivalenceElimination, 
   ruleIdempotency, ruleImplicationElimination, ruleFRuleComplement, ruleFRuleConjunction, ruleFRuleDisjunction, ruleFRuleNotT,
   ruleTRuleComplement, ruleTRuleConjunction, ruleTRuleDisjunction, ruleTRuleNotF, ruleCommutativeAbsorption :: Ord a => Eq a => LgcRule a

ruleAbsorption = createRule "Absorption" "single.absorption" absorption
ruleCommutativeAbsorption = createRule "Absorption" "single.commutative.and.absorption" f
   where
      f :: Ord a => LgcRed a
      f ((p :&&: q) :||: r) | p == r = Just r  -- check (((p :&&: q) :||: r) | p == r) .*. commutativity (lk) .*. absorption r -- absorption ((q :&&: p) :||: r) | otherwise = Nothing
      f (p :&&: (q :||: r)) | p == r = Just p -- .|. absorption (p :&&: (r :||: q)) | otherwise = Nothing
      f (p :||: (q :&&: r)) | p == q = Just p -- absorption ((r :&&: q) :||: p) | otherwise = Nothing 
      f ((p :||: q) :&&: r) | q == r = Just r -- absorption (r :&&: (q :||: p)) | otherwise = Nothing
      f _                            = Nothing
ruleAssociativity = createRule "Associativity" "single.associativity" associativity
ruleCommutativity = createRule "Commutativity" "single.commutativity" commutativity
ruleDeMorganAnd = createRule "De Morgan And" "single.demorgan.and" deMorganAnd
ruleDeMorganOr = createRule "De Morgan Or" "single.demorgan.or" deMorganOr
ruleDistributivity = createRule "Distributivity" "single.distributivity" distributivity
ruleDoubleNot = createRule "Double Not" "single.doublenot" doubleNot
ruleEquivalenceElimination = createRule "Equivalence Elimination" "single.equivalenceelimination" equivalenceElimination
ruleIdempotency = createRule "Idempotency" "single.idempotency" idempotency
ruleImplicationElimination = createRule "Implication Elimination" "single.implicationelimination" implicationElimination
ruleFRuleComplement = createRule "F-Rule Complement" "single.frulecomplement" fRuleComplement
ruleFRuleConjunction = createRule "F-Rule Conjunction" "single.fruleconjunction" fRuleConjunction
ruleFRuleDisjunction = createRule "F-Rule Disjunction" "single.fruledisjunction" fRuleDisjunction
ruleFRuleNotT = createRule "F-Rule Not T" "single.frulenott" fRuleNotT
ruleTRuleComplement = createRule "T-Rule Complement" "single.trulecomplement" tRuleComplement
ruleTRuleConjunction = createRule "T-Rule Conjunction" "single.truleconjunction" tRuleConjunction
ruleTRuleDisjunction = createRule "T-Rule Disjunction" "single.truledisjunction" tRuleDisjunction
ruleTRuleNotF = createRule "T-Rule Not F" "single.trulenotf" tRuleNotF

------------------------------------------------------------------------------------------------------------
-- Set of laws of Equivalence
-- Commutative variants of the rules absorption, distributiveness and the TT and FF rules are also allowed.
------------------------------------------------------------------------------------------------------------
absorption, associativity, commutativity, deMorganAnd, deMorganOr, doubleNot, distributivity, equivalenceElimination, 
   fRuleConjunction, fRuleComplement, fRuleNotT, fRuleDisjunction, idempotency, implicationElimination,
   tRuleConjunction, tRuleComplement, tRuleNotF, tRuleDisjunction :: Ord a => Eq a => LgcRed a

absorption ((p :&&: q) :||: r) | q == r = Just r 
absorption (p :&&: (q :||: r)) | p == q = Just p
absorption _                            = Nothing

associativity ((p :||: q) :||: r) = Just (p :||: (q:||: r))
associativity ((p :&&: q) :&&: r) = Just (p :&&: (q :&&: r))
associativity _                   = Nothing

commutativity (p :||: q) = Just (q :||: p)
commutativity (p :&&: q) = Just (q :&&: p) 
commutativity _          = Nothing

deMorganAnd (Not (p :&&: q)) = Just (Not p :||: Not q)
deMorganAnd _                = Nothing

deMorganOr  (Not (p :||: q)) = Just (Not p :&&: Not q)
deMorganOr  _                = Nothing  

-- Left distributivity
distributivity (p :||: (q :&&: r)) = Just ((p :||: q) :&&: (p :||: r))
-- Right distributivity
distributivity ((p :&&: q) :||: r) = Just ((p :||: r) :&&: (q :||: r))
distributivity _                   = Nothing 

doubleNot (Not (Not p)) = Just p
doubleNot _             = Nothing

equivalenceElimination (p :<->: q) = Just ((p :&&: q) :||: (Not p :&&: Not q))
equivalenceElimination _           = Nothing

fRuleConjunction (p :&&: F) = Just F
fRuleConjunction _          = Nothing

fRuleComplement (p :&&: Not q) | p == q = Just F
fRuleComplement _                       = Nothing 

fRuleNotT (Not T) = Just F
fRuleNotT _       = Nothing 

fRuleDisjunction (p :||: F) = Just p
fRuleDisjunction _          = Nothing

idempotency (p :||: q) | p == q    = Just p 
idempotency (p :&&: q) | p == q    = Just p 
idempotency _                      = Nothing

implicationElimination (p :->: q) = Just (Not p :||: q)
implicationElimination _          = Nothing

tRuleConjunction (p :&&: T) = Just p
tRuleConjunction _          = Nothing

tRuleComplement (p :||: Not q) | p == q = Just T
tRuleComplement _                       = Nothing 

tRuleNotF (Not F) = Just T
tRuleNotF _       = Nothing 

tRuleDisjunction (p :||: T) = Just T    
tRuleDisjunction _          = Nothing
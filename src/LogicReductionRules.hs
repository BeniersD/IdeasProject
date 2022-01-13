module LogicReductionRules 
   ( hasRule,  createRule, convertToRule,
     ruleAbsorption, ruleAssociativity, ruleCommutativity, ruleDeMorganAnd, ruleDeMorganOr, 
     ruleDoubleNot, ruleDistributivity, ruleEquivalenceElimination, ruleIdempotency, ruleImplicationElimination, 
     ruleFRuleComplement, ruleFRuleConjunction, ruleFRuleDisjunction, ruleFRuleNotT, ruleTRuleComplement, 
     ruleTRuleConjunction, ruleTRuleDisjunction, ruleTRuleNotF, commutativity
   ) where

import Ideas.Common.Library hiding (description)
import Ideas.Main.Default
import Domain.Logic.Formula
import Ideas.Utils.Prelude
import Data.Maybe
import Data.List

------------------------------------------------------------------------------------------------------------
-- Generic rewrite/reduction functions
------------------------------------------------------------------------------------------------------------
hasRule :: Rule (Logic a) -> Logic a -> Bool
hasRule x = isJust . apply x

createRule :: String -> String -> (Logic a -> Maybe (Logic a)) -> Rule (Logic a)
createRule x y f = describe ( "Rewrite " ++ x ) $ makeRule ( "rewrite." ++ y ) f

convertToRule :: String -> String -> LabeledStrategy (Logic a) -> Rule (Logic a)
convertToRule x y f = describe ( "Rewrite " ++ x ) $ makeRule ( "rewrite." ++ y ) (apply f)

------------------------------------------------------------------------------------------------------------
-- Set of rules
------------------------------------------------------------------------------------------------------------
ruleAbsorption, ruleAssociativity, ruleCommutativity, ruleDeMorganAnd, ruleDeMorganOr, ruleDoubleNot, ruleDistributivity, ruleEquivalenceElimination, 
   ruleIdempotency, ruleImplicationElimination, ruleFRuleComplement, ruleFRuleConjunction, ruleFRuleDisjunction, ruleFRuleNotT,
   ruleTRuleComplement, ruleTRuleConjunction, ruleTRuleDisjunction, ruleTRuleNotF :: Eq a => Rule (Logic a)

ruleAbsorption = createRule "Absorption" "single.absorption" absorption
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
   tRuleConjunction, tRuleComplement, tRuleNotF, tRuleDisjunction :: Eq a => Logic a -> Maybe (Logic a)

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
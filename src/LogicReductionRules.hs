module LogicReductionRules 
   ( hasRule,  createRule, convertToRule,
     ruleAbsorption, ruleAssociativity, ruleCommutativity, ruleDeMorganAnd, ruleDeMorganOr, 
     ruleDoubleNot, ruleDistributivity, ruleEquivalenceElimination, ruleIdempotency, ruleImplicationElimination, 
     ruleFRuleComplement, ruleFRuleConjunction, ruleFRuleDisjunction, ruleFRuleNotT, ruleTRuleComplement, 
     ruleTRuleConjunction, ruleTRuleDisjunction, ruleTRuleNotF, commutativity
   ) where

import Domain.Logic.Formula
import Ideas.Common.Library 
import LogicFunctions

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Set of rules
-------------------------------------------------------------------------------------------------------------------------------------------------
ruleAbsorption, ruleAssociativity, ruleCommutativity, ruleDeMorganAnd, ruleDeMorganOr, ruleDoubleNot, ruleDistributivity, 
   ruleEquivalenceElimination, ruleIdempotency, ruleImplicationElimination, ruleFRuleComplement, ruleFRuleConjunction, ruleFRuleDisjunction, 
   ruleFRuleNotT, ruleTRuleComplement, ruleTRuleConjunction, ruleTRuleDisjunction, ruleTRuleNotF :: Rule SLogic

ruleAbsorption             = createRule "Rewrite Absorption"              "single.absorption"             absorption
ruleAssociativity          = createRule "Rewrite Associativity"           "single.associativity"          associativity
ruleCommutativity          = createRule "Rewrite Commutativity"           "single.commutativity"          commutativity
ruleDeMorganAnd            = createRule "Rewrite DeMorgan And"            "single.demorgan.and"           deMorganAnd
ruleDeMorganOr             = createRule "Rewrite DeMorgan Or"             "single.demorgan.or"            deMorganOr
ruleDistributivity         = createRule "Rewrite Distributivity"          "single.distributivity"         distributivity
ruleDoubleNot              = createRule "Rewrite Double Not"              "single.doublenot"              doubleNot
ruleEquivalenceElimination = createRule "Rewrite Equivalence Elimination" "single.equivalenceelimination" equivalenceElimination
ruleIdempotency            = createRule "Rewrite Idempotency"             "single.idempotency"            idempotency
ruleImplicationElimination = createRule "Rewrite Implication Elimination" "single.implicationelimination" implicationElimination
ruleFRuleComplement        = createRule "Rewrite F-Rule Complement"       "single.frulecomplement"        fRuleComplement
ruleFRuleConjunction       = createRule "Rewrite F-Rule Conjunction"      "single.fruleconjunction"       fRuleConjunction
ruleFRuleDisjunction       = createRule "Rewrite F-Rule Disjunction"      "single.fruledisjunction"       fRuleDisjunction
ruleFRuleNotT              = createRule "Rewrite F-Rule Not T"            "single.frulenott"              fRuleNotT
ruleTRuleComplement        = createRule "Rewrite T-Rule Complement"       "single.trulecomplement"        tRuleComplement
ruleTRuleConjunction       = createRule "Rewrite T-Rule Conjunction"      "single.truleconjunction"       tRuleConjunction
ruleTRuleDisjunction       = createRule "Rewrite T-Rule Disjunction"      "single.truledisjunction"       tRuleDisjunction
ruleTRuleNotF              = createRule "Rewrite T-Rule Not F"            "single.trulenotf"              tRuleNotF

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Set of laws of Equivalence
-- Commutative variants of the rules distributiveness are added
-------------------------------------------------------------------------------------------------------------------------------------------------
absorption, associativity, commutativity, deMorganAnd, deMorganOr, doubleNot, distributivity, equivalenceElimination, 
   fRuleConjunction, fRuleComplement, fRuleNotT, fRuleDisjunction, idempotency, implicationElimination,
   tRuleConjunction, tRuleComplement, tRuleNotF, tRuleDisjunction :: SLogic -> Maybe (SLogic)

absorption ((p :&&: q) :||: r) | q == r = Just r 
absorption (p :&&: (q :||: r)) | p == q = Just p
absorption _                            = Nothing

associativity ((p :||: q) :||: r)       = Just (p :||: (q:||: r))
associativity ((p :&&: q) :&&: r)       = Just (p :&&: (q :&&: r))
associativity _                         = Nothing

commutativity (p :||: q)                = Just (q :||: p)
commutativity (p :&&: q)                = Just (q :&&: p) 
commutativity _                         = Nothing

deMorganAnd (Not (p :&&: q))            = Just (Not p :||: Not q)
deMorganAnd _                           = Nothing

deMorganOr  (Not (p :||: q))            = Just (Not p :&&: Not q)
deMorganOr  _                           = Nothing  

-- Left distributivity
distributivity (p :||: (q :&&: r))      = Just ((p :||: q) :&&: (p :||: r))
distributivity (p :&&: (q :||: r))      = Just ((p :&&: q) :||: (p :&&: r))
-- Right distributivity
distributivity ((p :&&: q) :||: r)      = Just ((p :||: r) :&&: (q :||: r))
distributivity ((p :||: q) :&&: r)      = Just ((p :&&: r) :||: (q :&&: r))
distributivity _                        = Nothing 

doubleNot (Not (Not p))                 = Just p
doubleNot _                             = Nothing

equivalenceElimination (p :<->: q)      = Just ((p :&&: q) :||: (Not p :&&: Not q))
equivalenceElimination _                = Nothing

fRuleConjunction (p :&&: F)             = Just F
fRuleConjunction _                      = Nothing

fRuleComplement (p :&&: Not q) | p == q = Just F
fRuleComplement _                       = Nothing 

fRuleNotT (Not T)                       = Just F
fRuleNotT _                             = Nothing 

fRuleDisjunction (p :||: F)             = Just p
fRuleDisjunction _                      = Nothing

idempotency (p :||: q) | p == q         = Just p 
idempotency (p :&&: q) | p == q         = Just p 
idempotency _                           = Nothing

implicationElimination (p :->: q)       = Just (Not p :||: q)
implicationElimination _                = Nothing

tRuleConjunction (p :&&: T)             = Just p
tRuleConjunction _                      = Nothing

tRuleComplement (p :||: Not q) | p == q = Just T
tRuleComplement _                       = Nothing 

tRuleNotF (Not F)                       = Just T
tRuleNotF _                             = Nothing 

tRuleDisjunction (p :||: T)             = Just T    
tRuleDisjunction _                      = Nothing
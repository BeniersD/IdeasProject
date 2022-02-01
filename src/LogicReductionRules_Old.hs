module LogicReductionRules_Old
    ( isDoubleNot, hasDoubleNot, singleDoubleNot, multiDoubleNot, multiDeMorgan, multiDeMorganAndMultiDoubleNot, multiImpl, multiAbsorp, multiIdemp, 
      multiFRuleConj, multiTRuleConj, multiFRuleDisj, multiTRuleDisj, multiFRuleCompl, multiTRuleCompl, multiTRuleNot, multiFRuleNot, 
      multiImplAndMultiDoubleNot, multiLogEq, multiLogEqAndMultiDoubleNot, multiAbsorpAndMultiDoubleNot, multiIdempAndMultiDoubleNot, 
      multiDeMorganAndMultiImpl, multiDeMorganAndMultiImplAndDoubleNot, multiDeMorganAndMultiLogEq, multiDeMorganAndMultiLogEqAndDoubleNot,
      multiReduc, (~=)
    ) where

import Control.Applicative
import Control.Monad
import Data.Foldable (toList)
import Data.List
import Data.Monoid
import Data.Function
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Typeable
import Domain.Algebra.Boolean
import Domain.Logic.Formula  hiding (not)
import Ideas.Utils.Uniplate
import Ideas.Common.Library
import Ideas.Main.Default

isDoubleNot :: SLogic -> Bool
isDoubleNot (Not (Not _)) = True
isDoubleNot _             = False

hasDoubleNot :: SLogic -> Bool
hasDoubleNot (Not (Not _)) = True
hasDoubleNot p             = any hasDoubleNot (children p) -- = or (map hasDoubleNot (children p))

singleDoubleNot :: SLogic -> SLogic
singleDoubleNot (Not (Not p)) =  p
singleDoubleNot p             =  p

multiReduc :: (SLogic -> SLogic) -> (SLogic -> SLogic) -> SLogic -> SLogic 
multiReduc f g p = f (descend g p)

multiDoubleNot:: SLogic -> SLogic
--multiDoubleNot (Not (Not p)) = multiDoubleNot p
--multiDoubleNot p = descend multiDoubleNot p
--multiDoubleNot p = f (descend multiDoubleNot p)
multiDoubleNot = multiReduc f multiDoubleNot
    where
        f (Not (Not p)) = f p
        f p             = p 

multiDeMorgan :: SLogic -> SLogic
multiDeMorgan = multiReduc f multiDeMorgan
    where
        f (Not (p :&&: q)) = f (Not p) :||: f (Not q)
        f (Not (p :||: q)) = f (Not p) :&&: f (Not q)
        f p                = p 

multiImpl :: SLogic -> SLogic
multiImpl = multiReduc f multiImpl
    where
        f (p :->: q) = f (Not p) :||: f q
        f p          = p 

multiLogEq :: SLogic -> SLogic
multiLogEq = multiReduc f multiLogEq
    where
        f (p :<->: q) = f (p :&&: q) :||: f (Not p :&&: Not q)
        f p           = p 

multiAbsorp :: SLogic -> SLogic
multiAbsorp = multiReduc f multiAbsorp
    where
        f ((p :&&: q) :||: r) = if q == r || p == r then f r else f (p :&&: q) :||: f r
        f (p :||: (q :&&: r)) = if q == r || p == r then f p else f p :||: f (q :&&: r)
        f (p :&&: (q :||: r)) = if p == q || p == r then f p else f p :&&: f (q :||: r)
        f ((p :||: q) :&&: r) = if p == r || q == r then f r else f (p :||: q) :&&: f r
        f p                   = p 

multiIdemp :: SLogic -> SLogic
multiIdemp = multiReduc f multiIdemp
    where
        f (p :||: q) = if p == q then f p else f p :||: f q
        f (p :&&: q) = if p == q then f p else f p :&&: f q
        f p          = p 

multiFRuleConj :: SLogic -> SLogic
multiFRuleConj = multiReduc f multiFRuleConj
    where
        f (p :&&: F) = F
        f (F :&&: p) = F        
        f p          = p 

multiTRuleConj :: SLogic -> SLogic
multiTRuleConj = multiReduc f multiTRuleConj
    where
        f (p :&&: T) = p
        f (T :&&: p) = p
        f p          = p 

multiFRuleDisj :: SLogic -> SLogic
multiFRuleDisj = multiReduc f multiFRuleDisj
    where
        f (p :||: F) = p
        f (F :||: p) = p        
        f p          = p 

multiTRuleDisj :: SLogic -> SLogic
multiTRuleDisj  = multiReduc f multiTRuleDisj
    where
        f (p :||: T) = T
        f (T :||: p) = T       
        f p          = p 

multiFRuleCompl :: SLogic -> SLogic
multiFRuleCompl = multiReduc f multiFRuleCompl
    where
        f (p :&&: Not q) = if p == q then F else f p :&&: f (Not q)
        f (Not p :&&: q) = if p == q then F else f (Not p) :&&: f q 
        f p              = p 

multiTRuleCompl :: SLogic -> SLogic
multiTRuleCompl = multiReduc f multiTRuleCompl
    where
        f (p :||: Not q) = if p == q then T else f p :||: f (Not q)
        f (Not p :||: q) = if p == q then T else f (Not p) :||: f q 
        f p              = p 

multiFRuleNot :: SLogic -> SLogic
multiFRuleNot p = multiReduc f multiFRuleNot p
    where
        f (Not T) = F
        f p       = p 

multiTRuleNot :: SLogic -> SLogic
multiTRuleNot p =  multiReduc f multiTRuleNot p
    where
        f (Not F) = T
        f p       = p 

multiDeMorganAndMultiDoubleNot :: SLogic -> SLogic
multiDeMorganAndMultiDoubleNot = multiDoubleNot . multiDeMorgan  

multiImplAndMultiDoubleNot :: SLogic -> SLogic
multiImplAndMultiDoubleNot = multiDoubleNot . multiImpl  

multiLogEqAndMultiDoubleNot :: SLogic -> SLogic
multiLogEqAndMultiDoubleNot = multiDoubleNot . multiLogEq

multiAbsorpAndMultiDoubleNot :: SLogic -> SLogic
multiAbsorpAndMultiDoubleNot = multiAbsorp . multiDoubleNot

multiIdempAndMultiDoubleNot :: SLogic -> SLogic
multiIdempAndMultiDoubleNot = multiIdemp . multiDoubleNot

multiDeMorganAndMultiImpl :: SLogic -> SLogic
multiDeMorganAndMultiImpl = multiDeMorgan . multiImpl

multiDeMorganAndMultiImplAndDoubleNot :: SLogic -> SLogic
multiDeMorganAndMultiImplAndDoubleNot = multiDoubleNot . multiDeMorgan . multiImpl  

multiDeMorganAndMultiLogEq :: SLogic -> SLogic
multiDeMorganAndMultiLogEq = multiDeMorgan . multiLogEq  

multiDeMorganAndMultiLogEqAndDoubleNot :: SLogic -> SLogic
multiDeMorganAndMultiLogEqAndDoubleNot = multiDoubleNot . multiDeMorgan . multiLogEq  

(~=) :: (SLogic -> SLogic) -> SLogic -> SLogic -> Bool
--(~=) = \f -> (\p -> (\q -> f p == f q))
(~=) f = (==) `on` f


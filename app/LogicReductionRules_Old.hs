module LogicReductionRules_Old
    (ChrLogic,Reduction, LChrLogic,
     isDoubleNot, hasDoubleNot, singleDoubleNot, multiDoubleNot, multiDeMorgan, multiDeMorganAndMultiDoubleNot, multiImpl, multiAbsorp, multiIdemp, 
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

type ChrLogic  = Logic Char
type Reduction = (ChrLogic -> ChrLogic)
type LChrLogic = [ChrLogic]

isDoubleNot :: Logic a -> Bool
isDoubleNot (Not (Not _)) = True
isDoubleNot _             = False

hasDoubleNot :: Logic a -> Bool
hasDoubleNot (Not (Not _)) = True
hasDoubleNot p             = any hasDoubleNot (children p) -- = or (map hasDoubleNot (children p))

singleDoubleNot :: Logic a -> Logic a
singleDoubleNot (Not (Not p)) =  p
singleDoubleNot p             =  p

multiReduc :: (Logic a -> Logic a) -> (Logic a -> Logic a) -> Logic a -> Logic a 
multiReduc f g p = f (descend g p)

multiDoubleNot:: Logic a -> Logic a
--multiDoubleNot (Not (Not p)) = multiDoubleNot p
--multiDoubleNot p = descend multiDoubleNot p
--multiDoubleNot p = f (descend multiDoubleNot p)
multiDoubleNot = multiReduc f multiDoubleNot
    where
        f (Not (Not p)) = f p
        f p             = p 

multiDeMorgan :: Logic a -> Logic a
multiDeMorgan = multiReduc f multiDeMorgan
    where
        f (Not (p :&&: q)) = f (Not p) :||: f (Not q)
        f (Not (p :||: q)) = f (Not p) :&&: f (Not q)
        f p                = p 

multiImpl :: Logic a -> Logic a
multiImpl = multiReduc f multiImpl
    where
        f (p :->: q) = f (Not p) :||: f q
        f p          = p 

multiLogEq :: Logic a -> Logic a
multiLogEq = multiReduc f multiLogEq
    where
        f (p :<->: q) = f (p :&&: q) :||: f (Not p :&&: Not q)
        f p           = p 

multiAbsorp :: Eq a => Logic a -> Logic a
multiAbsorp = multiReduc f multiAbsorp
    where
        f ((p :&&: q) :||: r) = if q == r || p == r then f r else f (p :&&: q) :||: f r
        f (p :||: (q :&&: r)) = if q == r || p == r then f p else f p :||: f (q :&&: r)
        f (p :&&: (q :||: r)) = if p == q || p == r then f p else f p :&&: f (q :||: r)
        f ((p :||: q) :&&: r) = if p == r || q == r then f r else f (p :||: q) :&&: f r
        f p                   = p 

multiIdemp :: Eq a => Logic a -> Logic a
multiIdemp = multiReduc f multiIdemp
    where
        f (p :||: q) = if p == q then f p else f p :||: f q
        f (p :&&: q) = if p == q then f p else f p :&&: f q
        f p          = p 

multiFRuleConj :: Logic a -> Logic a
multiFRuleConj = multiReduc f multiFRuleConj
    where
        f (p :&&: F) = F
        f (F :&&: p) = F        
        f p          = p 

multiTRuleConj :: Logic a -> Logic a
multiTRuleConj = multiReduc f multiTRuleConj
    where
        f (p :&&: T) = p
        f (T :&&: p) = p
        f p          = p 

multiFRuleDisj :: Logic a -> Logic a
multiFRuleDisj = multiReduc f multiFRuleDisj
    where
        f (p :||: F) = p
        f (F :||: p) = p        
        f p          = p 

multiTRuleDisj :: Logic a -> Logic a
multiTRuleDisj  = multiReduc f multiTRuleDisj
    where
        f (p :||: T) = T
        f (T :||: p) = T       
        f p          = p 

multiFRuleCompl :: Eq a => Logic a -> Logic a
multiFRuleCompl = multiReduc f multiFRuleCompl
    where
        f (p :&&: Not q) = if p == q then F else f p :&&: f (Not q)
        f (Not p :&&: q) = if p == q then F else f (Not p) :&&: f q 
        f p              = p 

multiTRuleCompl :: Eq a => Logic a -> Logic a
multiTRuleCompl = multiReduc f multiTRuleCompl
    where
        f (p :||: Not q) = if p == q then T else f p :||: f (Not q)
        f (Not p :||: q) = if p == q then T else f (Not p) :||: f q 
        f p              = p 

multiFRuleNot :: Logic a -> Logic a
multiFRuleNot p = multiReduc f multiFRuleNot p
    where
        f (Not T) = F
        f p       = p 

multiTRuleNot :: Logic a -> Logic a
multiTRuleNot p =  multiReduc f multiTRuleNot p
    where
        f (Not F) = T
        f p       = p 

multiDeMorganAndMultiDoubleNot :: Logic a -> Logic a
multiDeMorganAndMultiDoubleNot = multiDoubleNot . multiDeMorgan  

multiImplAndMultiDoubleNot :: Logic a -> Logic a
multiImplAndMultiDoubleNot = multiDoubleNot . multiImpl  

multiLogEqAndMultiDoubleNot :: Logic a -> Logic a
multiLogEqAndMultiDoubleNot = multiDoubleNot . multiLogEq

multiAbsorpAndMultiDoubleNot :: Eq a => Logic a -> Logic a
multiAbsorpAndMultiDoubleNot = multiAbsorp . multiDoubleNot

multiIdempAndMultiDoubleNot :: Eq a => Logic a -> Logic a
multiIdempAndMultiDoubleNot = multiIdemp . multiDoubleNot

multiDeMorganAndMultiImpl :: Logic a -> Logic a
multiDeMorganAndMultiImpl = multiDeMorgan . multiImpl

multiDeMorganAndMultiImplAndDoubleNot :: Logic a -> Logic a
multiDeMorganAndMultiImplAndDoubleNot = multiDoubleNot . multiDeMorgan . multiImpl  

multiDeMorganAndMultiLogEq :: Logic a -> Logic a
multiDeMorganAndMultiLogEq = multiDeMorgan . multiLogEq  

multiDeMorganAndMultiLogEqAndDoubleNot :: Logic a -> Logic a
multiDeMorganAndMultiLogEqAndDoubleNot = multiDoubleNot . multiDeMorgan . multiLogEq  

(~=) :: Eq a => (Logic a -> Logic a) -> Logic a -> Logic a -> Bool
--(~=) = \f -> (\p -> (\q -> f p == f q))
(~=) f = (==) `on` f


module LogicTestCases
     where

import Domain.Logic.Formula 
import Ideas.Common.Library
import Ideas.Main.Default
import Ideas.Utils.Prelude (ShowString, subsets)

o = Var "o"
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"

commutativityTestSet, doubleNotTestSet, deMorganAndTestSetSimple, deMorganOrTestSetSimple, deMorganOrTestSetComplex, deMorganAndTestSetComplex, deMorganAndDoubleNotTestSet,
  implicationEliminationTestSet, implicationEliminationDerivTestSet , absorptionTestSet, idempotencyTestSet, boolRuleConjunctionTestSet, boolRuleDisjunctionTestSet,
  boolRuleComplementTestSet, boolRuleNotTestSet, deMorganAndImplicationEliminationTestSet, deMorganAndEquivalenceEliminationTestSet,
  equivalenceEliminationTestSet, deMorganDerivTestSet, equivalenceEliminationDerivTestSet :: [Logic String]
commutativityTestSet =
                [ p :&&: q,
                  q :&&: p,
                  p :&&: Not p,
                  q :&&: Not p,
                  p :&&: Not q,
                  Not q :&&: p,                  
                  Not p :&&: q,                  
                  Not p :&&: Not q,                  
                  Not q :&&: Not p,                  
                  p :&&: Not (Not p),
                  Not (Not p :&&: p),
                  T :&&: F,
                  F :&&: T,
                  T :&&: p,
                  p :&&: T,
                  F :&&: p,
                  p :&&: F, 
                  p :&&: (q :&&: r),
                  p :||: (q :||: r),
                  p :&&: (q :&&: p),
                  p :&&: (p :||: q),
                  (p :&&: q) :&&: p,
                  (p :&&: q) :&&: (q :&&: p),
                  p :||: q,
                  q :||: p,
                  p :||: Not (Not p),
                  Not (Not p :||: p),
                  T :||: F,
                  F :||: T,
                  T :||: p,
                  p :||: T,
                  F :||: p,
                  p :||: F, 
                  p :||: (p :||: q),
                  (p :||: q) :||: p,
                  (p :||: q) :||: (q :||: p)
                ]

implicationEliminationDerivTestSet =
                [ p :->: q,
                  p :->: Not (Not p),
                  p :->: (p :&&: q),
                  T :->: F,
                  F :->: T,
                  T :->: p,
                  F :->: p,
                  p :->: T,
                  p :->: F,
                  T :->: Not (Not p),
                  Not (Not p) :->: T,
                  (p :&&: q) :->: F,
                  (p :||: q) :->: T,              
                  (p :->: T) :&&: (F :->: p),
                  (T :->: Not (Not p)) :&&: (Not p :->: F),
                  (Not (Not p) :->: T) :&&: (Not p :->: T),
                  (T :->: F) :->: (F :->: T)
                ]

equivalenceEliminationDerivTestSet = 
                [
                 Not (p :<->: q), 
                 Not (p :<->: Not (Not q)),
                 p :<->: Not (Not p),
                 Not (F :<->: T),
                 Not (T :<->: F),
                 Not (F :<->: q),
                 Not (Not (Not q)) :<->: T,
                 (p :<->: T) :&&: (F :<->: p),
                 (F :<->: Not (Not p)) :&&: (Not p :<->: T),
                 (Not (Not p) :<->: Not (Not p)) :&&: (F :<->: Not p),
                 (p :<->: Not (Not p)) :<->: (Not p :<->: T)
                ]

deMorganDerivTestSet =
                [ 
                  Not (p :&&: q),                                   -- ¬(p ˄ q)  
                  Not (q :&&: p),                                   -- ¬(q ˄ p)  
                  Not (q :&&: p :&&: r),                      -- ¬(q ˄ p ˄ r)  
                  Not (q :&&: p :&&: r :&&: s),         -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (Not (q :&&: p)),                             -- ¬¬(q ˄ p)  
                  Not (Not (Not p) :&&: T),                             -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :&&: T :&&: T),                      -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :&&: T :&&: F),                      -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :&&: T :&&: Not (Not p)),    -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :&&: T :&&: Not (Not q)),    -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :&&: Not p :&&: T),          -- ¬(¬¬p ˄ ¬p ˄ T)
                  Not (T :||: Not (Not p)),                             -- ¬(T ˅ ¬¬p)
                  Not (q :||: p :||: r),                      -- ¬(q ˅ p ˅ r)  
                  Not (q :||: p :||: r :||: s),         -- ¬(q ˅ p ˅ r ˅ s)                  
                  Not (Not (Not p) :||: T),                             -- ¬(¬¬p ˅ T)
                  Not (Not (Not p) :||: T :||: T),                      -- ¬(¬¬p ˅ T ˅ T)        
                  Not (Not (Not p) :||: T :||: F),                      -- ¬(¬¬p ˅ T ˅ F)                
                  Not (T :||: Not (Not p) :||: Not (Not p)),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not p)),    -- ¬(¬¬p ˅ T ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not q)),    -- ¬(¬¬p ˅ T ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: T),          -- ¬(¬¬p ˅ ¬p ˅ T)
                  Not (F :&&: Not (Not p)),                             -- ¬(T ˄ ¬¬p)
                  Not (Not (Not p) :&&: F),                             -- ¬(¬¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: F),                      -- ¬(¬¬p ˄ F ˄ F)        
                  Not (Not (Not p) :&&: F :&&: T),                      -- ¬(¬¬p ˄ F ˄ T)                
                  Not (Not (Not p) :&&: F :&&: Not (Not p)),    -- ¬(¬¬p ˄ F ˄ ¬¬p)    
                  Not (Not (Not p) :&&: F :&&: Not (Not q)),    -- ¬(¬¬p ˄ F ˄ ¬¬q)        
                  Not (F :&&: Not (Not p) :&&: Not p),          -- ¬(F ˄ ¬¬p ˄ ¬p)        
                  Not (Not (Not p) :&&: Not p :&&: F),          -- ¬(¬¬p ˄ ¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: Not p),          -- ¬(¬¬p ˄ F ˄ ¬p)                
                  Not (F :||: Not (Not p)),                             -- ¬(T ˅ ¬¬p)
                  Not (F :||: Not (Not p) :||: Not (Not p)),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)            
                  Not (Not (Not p) :||: F),                             -- ¬(¬¬p ˅ F)
                  Not (Not (Not p) :||: F :||: F),                      -- ¬(¬¬p ˅ F ˅ F)        
                  Not (Not (Not p) :||: F :||: T),                      -- ¬(¬¬p ˅ F ˅ T)                
                  Not (Not (Not p) :||: F :||: Not (Not p)),    -- ¬(¬¬p ˅ F ˅ ¬¬p)    
                  Not (Not (Not p) :||: F :||: Not (Not q)),    -- ¬(¬¬p ˅ F ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: F)           -- ¬(¬¬p ˅ ¬p ˅ F)
                ]

doubleNotTestSet =
                [ Not p, 
                  Not (Not p),
                  Not (Not p) :&&: Not( Not p),
                  Not (Not p) :&&: Not( Not p) :||: Not( Not( Not( Not p))),
                  Not (Not p) :||: Not( Not p) :||: Not( Not( Not( Not p))),
                  Not (Not p) :&&: Not( Not p) :&&: Not( Not( Not( Not p))) :||: Not( Not( Not( Not p))),
                  Not (Not p) :||: Not( Not p) :||: Not( Not( Not( Not p))) :||: Not( Not( Not( Not p))),
                  Not (Not (Not (Not (Not (Not p) :||: Not( Not p))))),
                  Not (Not (Not (Not p) :||: Not (Not p) :||: Not (Not (Not (Not p))))),
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p),
                  (p :->: Not (Not p) :<->: Not (Not p)) :<->: (Not p:<->: Not p),
                  (p :->: Not (Not (Not p):<->: Not (Not p))) :<->: (Not p:<->: Not (Not p))
                ]

deMorganAndTestSetSimple = 
                [ Not (p :&&: q),                                       -- ¬(q ˄ p)  
                  Not (q :&&: p :&&: r),                          -- ¬(q ˄ p ˄ r)  
                  Not (q :&&: p :&&: r :&&: s),             -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (q :&&: p :&&: r :&&: s :&&: t) -- ¬(q ˄ p ˄ r ˄ s ˄ t)                  
                ]

deMorganOrTestSetSimple =                   
                [ Not (p :||: q),                                       -- ¬(q ˅ p)  
                  Not (q :||: p :||: r),                          -- ¬(q ˅ p ˅ r)  
                  Not (q :||: p :||: r :||: s),             -- ¬(q ˅ p ˅ r ˅ s)  
                  Not (q :||: p :||: r :||: s :||: t) -- ¬(q ˅ p ˅ r ˅ s ˅ t)                  
                ]

deMorganAndTestSetComplex = 
                [ 
                  Not (Not (p :&&: q)),                                                                          -- ¬¬(p ˄ q)  
                  Not (Not (q :&&: p)),                                                                          -- ¬¬(q ˄ p)  
                  Not (Not (q :&&: p :&&: r)),                                                             -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r),                                                                   -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r :||: s),                                                      -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (q :||: p :||: s :&&: r),                                                      -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (p :->: Not (Not p :<->: Not (Not p))) :<->: Not (Not p :&&: Not p), -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (p :&&: q) :||: Not (p :&&: q),                                                -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (p :&&: q) :||: Not( p :&&: q)),                                          -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (p :&&: q)) :||: Not (Not (p :&&: q)),                                    -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (p :&&: q) :||: Not (p :&&: q)),                                          -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (p :&&: q) :||: Not (p :&&: q)) :||: p),                       -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not p) :&&: T),                                                                          -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :&&: T :&&: T),                                                                   -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :&&: T :&&: F),                                                                   -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :&&: T :&&: Not (Not p)),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :&&: T :&&: Not (Not q)),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :&&: Not p :&&: T),                                                       -- ¬(¬¬p ˄ ¬p ˄ T)
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p)                               -- ¬(¬¬p ˄ ¬p ˄ T)
                ]

deMorganOrTestSetComplex = 
                [ 
                  Not (Not (p :||: q)),                                                                          -- ¬¬(p ˄ q)  
                  Not (Not (q :||: p)),                                                                          -- ¬¬(q ˄ p)  
                  Not (Not (q :||: p :||: r)),                                                             -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r),                                                                   -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r :||: s),                                                      -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (q :||: p :||: s :&&: r),                                                      -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (p :->: Not (Not p :<->: Not (Not p))) :<->: Not (Not p :||: Not p), -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (p :||: q) :||: Not (p :||: q),                                                -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (p :||: q) :||: Not( p :||: q)),                                          -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (p :||: q)) :||: Not (Not (p :||: q)),                                    -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (p :||: q) :||: Not (p :||: q)),                                          -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (p :||: q) :||: Not (p :||: q)) :||: p),                       -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not p) :||: T),                                                                          -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :||: T :||: T),                                                                   -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :||: T :||: F),                                                                   -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :||: T :||: Not (Not p)),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not q)),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :||: Not p :&&: T),                                                       -- ¬(¬¬p ˄ ¬p ˄ T)
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p)                               -- ¬(¬¬p ˄ ¬p ˄ T)
                ]

deMorganAndDoubleNotTestSet =
                [ p,
                  Not p,
                  Not (Not p),
                  Not (p :&&: q), 
                  Not (p :&&: q) :||: Not (p :&&: q),
                  Not (Not (p :&&: q) :||: Not( p :&&: q)),
                  Not (Not (p :&&: q)) :||: Not (Not (p :&&: q)),
                  Not (Not (p :&&: q) :||: Not (p :&&: q)),
                  Not (Not (Not (p :&&: q) :||: Not (p :&&: q)) :||: p),
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p),
                  (p :->: Not (Not p :<->: Not (Not p))) :<->: (Not p :<->: Not p)
                ]

implicationEliminationTestSet =
                [
                  p :->: Not (Not p),
                  (p :->: p) :&&: (p :->: p),
                  (p :->: Not (Not p)) :&&: (Not p :->: Not p),
                  (Not (Not p) :->: Not (Not p)) :&&: (Not p :->: Not p),
                  (p :->: Not (Not p)) :->: (Not p :->: Not p)
                ]

equivalenceEliminationTestSet =
                [
                 p :<->: Not (Not p),
                 (p :<->: p) :&&: (p :<->: p),
                 (p :<->: Not (Not p)) :&&: (Not p :<->: Not p),
                 (Not (Not p) :<->: Not (Not p)) :&&: (Not p :<->: Not p),
                 (p :<->: Not (Not p)) :<->: (Not p :<->: Not p)
                ]

absorptionTestSet =
                [
                 (p :&&: q) :||: r,
                 p :&&: Not q :||: r,
                 (p :&&: q) :||: q,
                 (p :&&: Not q) :||: Not q,
                 r :||: (p :&&: q),
                 Not (Not r) :||: (p :&&: q),
                 p :||: (p :&&: q),
                 Not (Not p) :||: (p :&&: q),
                 q :||: (p :&&: q),
                 q :||: (p :&&: Not( Not q)),
                 p :&&: (p :||: q),
                 p :&&: (p :||: q),
                 q :&&: (p :||: q),
                 Not (Not q) :&&: (Not (Not p) :||: Not( Not q)),
                 (p :||: q) :&&: p,
                 (p :||: q) :&&: Not (Not p),
                 (p :||: q) :&&: q,
                 (Not (Not p) :||: Not (Not q)) :&&: Not (Not q),
                 ((p :||: q) :&&: p) :&&: ((p :&&: q) :||: q),
                 ((p :&&: q) :||: q) :&&: ((p :&&: q) :||: q),
                 (((p :||: q) :&&: q) :&&: ((p :&&: q) :||: q)) :||: q
                ]

idempotencyTestSet =
                [
                 p :&&: p,
                 Not( Not p) :&&: p,
                 p :||: p,
                 p :||: Not (Not p),
                 (p :&&: q) :&&: (p :&&: q),
                 (Not (Not p) :&&: q) :&&: (Not (Not p) :&&: Not (Not q)),
                 (p :||: p) :&&: (p :||: p),
                 (p :||: p) :&&: (p :||: Not (Not p)),
                 (p :&&: p) :&&: (p :||: p),
                 (p :&&: Not (Not p)) :&&: (p :||: Not (Not p)),
                 (p :&&: q) :&&: (q :&&: p),
                 (p :&&: Not (Not q)) :&&: (q :&&: Not (Not p)),
                 (p :&&: p) :&&: p,
                 (Not (Not p) :&&: Not (Not p)) :&&: Not (Not p)
                ]

boolRuleConjunctionTestSet =
                [
                 p :&&: F,
                 p :&&: T,
                 F :&&: p,
                 T :&&: p,
                 (F :&&: p) :&&: (p :&&: F),
                 (T :&&: p) :&&: (p :&&: T),
                 (F :&&: p) :&&: p,
                 (T :&&: p) :&&: p,
                 p :&&: (F :&&: p), 
                 p :&&: (T :&&: p),
                 (F :&&: p) :&&: (p :&&: F) :&&: p,
                 (T :&&: p) :&&: (p :&&: T) :&&: p
                ]

boolRuleDisjunctionTestSet =
                [
                 p :||: F,
                 p :||: T,
                 F :||: p,
                 T :||: p,
                 (F :||: p) :||: (p :||: F),
                 (T :||: p) :||: (p :||: T),
                 (F :||: p) :||: p,
                 (T :||: p) :||: p,
                 p :&&: (F :||: p),
                 p :||: (T :||: p), 
                 (F :||: p) :||: (p :||: F) :||: p,
                 (T :||: p) :||: (p :||: T) :||: p
                ]

boolRuleComplementTestSet =
                [
                 p :&&: Not p,
                 p :||: Not p,
                 Not p :&&: p,
                 Not p :||: p,
                 (Not p :&&: p) :&&: T,
                 (Not p :||: p) :||: F
                ]

boolRuleNotTestSet =
                [
                 Not T,
                 Not F,
                 p :&&: Not T,
                 p :&&: Not F,
                 p :&&: Not (Not T),
                 p :&&: Not (Not F),
                 p :&&: Not T :||: Not T,
                 p :&&: Not F :||: Not F
                ]

deMorganAndImplicationEliminationTestSet =
                [
                 Not (p :->: q),
                 Not (p :->: Not (Not q)),
                 Not (p :->: Not (Not q)) :||: Not (Not p :->: Not (Not q)),
                 Not (Not (p :->: Not (Not q)) :->: Not (Not p :->: Not (Not q)))
                ]

deMorganAndEquivalenceEliminationTestSet =
                [
                 Not (p :<->: q),
                 Not (p :<->: Not (Not q)),
                 Not (p :<->: Not (Not q)) :||: Not (Not p :<->: Not (Not q)),
                 Not (Not (p :<->: Not (Not q)) :<->: Not (Not p :<->: Not (Not q)))
                ]

associativityTestSet =
                [ 
                  (q :&&: p) :&&: r,                                 -- (q ˄ p) ˄ r
                  Not ((q :&&: p) :&&: r),                           -- ¬((q ˄ p) ˄ r)  
                  (q :||: p) :||: r,                                 -- (q ˅ p) ˅ r
                  Not ((q :||: p) :||: r),                           -- ¬((q ˅ p) ˅ r)  
                  ((q :||: p) :||: r) :||: s,                  -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((q :||: p) :||: r) :||: s),            -- ¬((q ˅ p) ˅ r) ˅ s)  
                  ((q :||: p) :||: r) :||: s,                  -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((q :||: p) :||: r) :||: s),            -- ¬((q ˅ p) ˅ r) ˅ s)  
                  (Not (Not p) :&&: T) :&&: T,                                 -- (¬¬p ˄ T) ˄ T
                  Not ((Not (Not p) :&&: T) :&&: F),                           -- ¬((¬¬p ˄ T) ˄ F)                
                  Not ( T :&&: (Not (Not p) :&&: T) :&&: Not (Not p)), -- ¬(T ˄ (¬¬p ˄ T) ˄ ¬¬p)    
                  Not ((Not (Not p) :||: T) :||: T :||: Not (Not q))   -- ¬((¬¬p ˄ T) ˄ T ˄ ¬¬q)        
                ]

distributivityTestSet =
                [ 
                  p :||: (q :&&: r),                                -- p ˅ (q ˄ r)
                  (p :&&: q) :||: r,                                -- (p ˄ q) ˅ r)
                  Not (p :||: (q :&&: r)),                          -- ¬(p ˅ (q ˄ r))
                  Not ((p :&&: q) :||: r),                          -- ¬((p ˄ q) ˅ r)
                  ((p :&&: q) :||: r) :||: s,                 -- ((p ˄ q) ˅ r) ˅ s
                  o :&&: ((p :&&: q) :||: r),                 -- o ˄ ((p ˄ q) ˅ r)
                  o :||: ((p :&&: q) :||: r),                 -- o ˅ ((p ˄ q) ˅ r)
                  o :||: (((p :&&: q) :||: r) :&&: s),  -- o ˅ ((p ˄ q) ˅ r) ˄ s
                  o :&&: (((p :&&: q) :||: r) :||: s),  -- o ˄ ((p ˄ q) ˅ r) ˅ s
                  o :||: (((p :&&: q) :||: r) :||: s)   -- o ˅ ((p ˄ q) ˅ r) ˅ s
                ]

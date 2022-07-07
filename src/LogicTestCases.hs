module LogicTestCases 
     where

import Domain.Logic.Formula
import LogicConstants

--------------------------------------------------------------------------------------------------------------------------------------
-- Test sets to evaluate rules and/or strategies
--------------------------------------------------------------------------------------------------------------------------------------
commutativityTestSet, doubleNotTestSet, deMorganAndTestSetSimple, deMorganOrTestSetSimple, deMorganOrTestSetComplex, deMorganAndTestSetComplex, deMorganAndDoubleNotTestSet,
  implicationEliminationTestSet, implicationEliminationDerivTestSet , absorptionTestSet, idempotencyTestSet, boolRuleConjunctionTestSet, boolRuleDisjunctionTestSet,
  boolRuleComplementTestSet, boolRuleNotTestSet, deMorganAndImplicationEliminationTestSet, deMorganAndEquivalenceEliminationTestSet,
  equivalenceEliminationTestSet, deMorganDerivTestSet, equivalenceEliminationDerivTestSet :: [SLogic]
commutativityTestSet =
                [ p :&&: q,                        -- 0. (p ˄ q)  
                  q :&&: p,                        -- 1. (q ˄ p)  
                  p :&&: Not p,                    -- 2. (p ˄ ¬p)
                  q :&&: Not p,                    -- 3. (q ˄ ¬p)  
                  p :&&: Not q,                    -- 4. (p ˄ ¬q)
                  Not q :&&: p,                    -- 5.
                  Not p :&&: q,                    -- 6.
                  Not p :&&: Not q,                -- 7.
                  Not q :&&: Not p,                -- 8.  
                  p :&&: Not (Not p),              -- 10.
                  Not (Not p :&&: p),              -- 11.
                  T :&&: F,                        -- 12. 
                  F :&&: T,                        -- 13.
                  T :&&: p,                        -- 14.  
                  p :&&: T,                        -- 15.
                  F :&&: p,                        -- 16.  
                  p :&&: F,                        -- 17.
                  p :&&: (q :&&: r),               -- 18.
                  p :||: (q :||: r),               -- 19.
                  p :&&: (q :&&: p),               -- 20.
                  p :&&: (p :||: q),               -- 21.
                  (p :&&: q) :&&: p,               -- 22.
                  (p :&&: q) :&&: (q :&&: p),      -- 23.
                  p :||: q,                        -- 24.
                  q :||: p,                        -- 25.
                  p :||: Not (Not p),              -- 26.
                  Not (Not p :||: p),              -- 27.
                  T :||: F,                        -- 28.
                  F :||: T,                        -- 29.
                  T :||: p,                        -- 30.
                  p :||: T,                        -- 31.
                  F :||: p,                        -- 32.
                  p :||: F,                        -- 33.
                  p :||: (p :||: q),               -- 34.
                  (p :||: q) :||: p,               -- 35.
                  (p :||: q) :||: (q :||: p),       -- 36.
                  (q :||: p) :||: (q :||: p),       -- 37.
                  (q :||: p) :||: (q :||: p) :||: (Not q :||: Not p) :||: (F :||: T)      -- 38.
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
                  Not (p :&&: q),                               -- ¬(p ˄ q)  
                  Not (q :&&: p),                               -- ¬(q ˄ p)  
                  Not (q :&&: p :&&: r),                        -- ¬(q ˄ p ˄ r)  
                  Not (q :&&: p :&&: r :&&: s),                 -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (Not (q :&&: p)),                         -- ¬¬(q ˄ p)  
                  Not (Not (Not p) :&&: T),                     -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :&&: T :&&: T),              -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :&&: T :&&: F),              -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :&&: T :&&: Not (Not p)),    -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :&&: T :&&: Not (Not q)),    -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :&&: Not p :&&: T),          -- ¬(¬¬p ˄ ¬p ˄ T)
                  Not (T :||: Not (Not p)),                     -- ¬(T ˅ ¬¬p)
                  Not (q :||: p :||: r),                        -- ¬(q ˅ p ˅ r)  
                  Not (q :||: p :||: r :||: s),                 -- ¬(q ˅ p ˅ r ˅ s)                  
                  Not (Not (Not p) :||: T),                     -- ¬(¬¬p ˅ T)
                  Not (Not (Not p) :||: T :||: T),              -- ¬(¬¬p ˅ T ˅ T)        
                  Not (Not (Not p) :||: T :||: F),              -- ¬(¬¬p ˅ T ˅ F)                
                  Not (T :||: Not (Not p) :||: Not (Not p)),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not p)),    -- ¬(¬¬p ˅ T ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not q)),    -- ¬(¬¬p ˅ T ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: T),          -- ¬(¬¬p ˅ ¬p ˅ T)
                  Not (F :&&: Not (Not p)),                     -- ¬(F ˄ ¬¬p)
                  Not (Not (Not p) :&&: F),                     -- ¬(¬¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: F),              -- ¬(¬¬p ˄ F ˄ F)        
                  Not (Not (Not p) :&&: F :&&: T),              -- ¬(¬¬p ˄ F ˄ T)                
                  Not (Not (Not p) :&&: F :&&: Not (Not p)),    -- ¬(¬¬p ˄ F ˄ ¬¬p)    
                  Not (Not (Not p) :&&: F :&&: Not (Not q)),    -- ¬(¬¬p ˄ F ˄ ¬¬q)        
                  Not (F :&&: Not (Not p) :&&: Not p),          -- ¬(F ˄ ¬¬p ˄ ¬p)        
                  Not (Not (Not p) :&&: Not p :&&: F),          -- ¬(¬¬p ˄ ¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: Not p),          -- ¬(¬¬p ˄ F ˄ ¬p)                
                  Not (F :||: Not (Not p)),                     -- ¬(T ˅ ¬¬p)
                  Not (F :||: Not (Not p) :||: Not (Not p)),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)            
                  Not (Not (Not p) :||: F),                     -- ¬(¬¬p ˅ F)
                  Not (Not (Not p) :||: F :||: F),              -- ¬(¬¬p ˅ F ˅ F)        
                  Not (Not (Not p) :||: F :||: T),              -- ¬(¬¬p ˅ F ˅ T)                
                  Not (Not (Not p) :||: F :||: Not (Not p)),    -- ¬(¬¬p ˅ F ˅ ¬¬p)    
                  Not (Not (Not p) :||: F :||: Not (Not q)),    -- ¬(¬¬p ˅ F ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: F)           -- ¬(¬¬p ˅ ¬p ˅ F)
                ]

doubleNotTestSet =
                [ Not p, 
                  Not (Not p),
                  Not (Not (p :&&: q)),
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
                [ Not (p :&&: q),                      -- ¬(q ˄ p)  
                  Not (q :&&: p :&&: r),               -- ¬(q ˄ p ˄ r)  
                  Not (q :&&: p :&&: r :&&: s),        -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (q :&&: p :&&: r :&&: s :&&: t)  -- ¬(q ˄ p ˄ r ˄ s ˄ t)                  
                ]

deMorganOrTestSetSimple =                   
                [ Not (p :||: q),                      -- ¬(q ˅ p)  
                  Not (q :||: p :||: r),               -- ¬(q ˅ p ˅ r)  
                  Not (q :||: p :||: r :||: s),        -- ¬(q ˅ p ˅ r ˅ s)  
                  Not (q :||: p :||: r :||: s :||: t)  -- ¬(q ˅ p ˅ r ˅ s ˅ t)                  
                ]

deMorganAndTestSetComplex = 
                [ 
                  Not (Not (p :&&: q)),                                                 -- ¬¬(p ˄ q)  
                  Not (Not (q :&&: p)),                                                 -- ¬¬(q ˄ p)  
                  Not (Not (q :&&: p :&&: r)),                                          -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r),                                                -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r :||: s),                                         -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (q :||: p :||: s :&&: r),                                         -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (p :->: Not (Not p :<->: Not (Not p))) :<->: Not (Not p :&&: Not p),  -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (p :&&: q) :||: Not (p :&&: q),                                   -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (p :&&: q) :||: Not( p :&&: q)),                             -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (p :&&: q)) :||: Not (Not (p :&&: q)),                       -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (p :&&: q) :||: Not (p :&&: q)),                             -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (p :&&: q) :||: Not (p :&&: q)) :||: p),                -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not p) :&&: T),                                             -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :&&: T :&&: T),                                      -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :&&: T :&&: F),                                      -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :&&: T :&&: Not (Not p)),                            -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :&&: T :&&: Not (Not q)),                            -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :&&: Not p :&&: T),                                  -- ¬(¬¬p ˄ ¬p ˄ T)
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p),                        -- ¬(¬¬p ˄ ¬p ˄ T)
                  Not (Not (Not p) :&&: Not p :&&: T) :<->: (Not p :<->: Not p)         -- ¬(¬¬p ˄ ¬p ˄ T) ↔ (¬p ↔ ¬p)
                ]

deMorganOrTestSetComplex = 
                [ 
                  Not (Not (p :||: q)),                                                 -- ¬¬(p ˄ q)  
                  Not (Not (q :||: p)),                                                 -- ¬¬(q ˄ p)  
                  Not (Not (q :||: p :||: r)),                                          -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r),                                                -- ¬¬(q ˄ p ˄ r)  
                  Not (q :||: p :&&: r :||: s),                                         -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (q :||: p :||: s :&&: r),                                         -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (p :->: Not (Not p :<->: Not (Not p))) :<->: Not (Not p :||: Not p),  -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (p :||: q) :||: Not (p :||: q),                                   -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (p :||: q) :||: Not( p :||: q)),                             -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (p :||: q)) :||: Not (Not (p :||: q)),                       -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (p :||: q) :||: Not (p :||: q)),                             -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (p :||: q) :||: Not (p :||: q)) :||: p),                -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not p) :||: T),                                             -- ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :||: T :||: T),                                      -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :||: T :||: F),                                      -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :||: T :||: Not (Not p)),                            -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not q)),                            -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :||: Not p :&&: T),                                  -- ¬(¬¬p ˄ ¬p ˄ T)
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p),                       -- ¬(¬¬p ˄ ¬p ˄ T)
                  (Not p :<->: Not p) :<->: Not (Not (Not p) :||: Not p :&&: T),        -- (¬p ↔ ¬p) ↔ ¬(¬¬p ˅ ¬p ˄ T)
                  Not (Not (Not p) :&&: Not p :&&: T :||: Not p) :<->: Not p            -- ¬(¬¬p ˄ ¬p ˄ T ˅ ¬p) ↔ ¬p
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
                  (p :&&: q) :||: r,                                        -- 0. No derivation    
                  p :&&: Not q :||: r,                                      -- 1. No derivation
                  q :||: (p :&&: Not( Not q)),                              -- 2. No derivation                
                  r :||: (p :&&: q),                                        -- 3. No derivation
                  Not (Not r) :||: (p :&&: q),                              -- 4. No derivation
                  Not (Not p) :||: (p :&&: q),                              -- 5. No derivation
                  (p :||: q) :&&: Not (Not p),                              -- 6. No derivation
                  (p :&&: q) :||: q,                                        -- 7. Absorption
                  (p :&&: Not q) :||: Not q,                                -- 8. Absorption
                  p :&&: (p :||: q),                                        -- 9. Absorption
                  (p :&&: q) :||: p,                                        -- 10. Commutative Absorption 1
                  (p :||: q) :&&: p,                                        -- 11. Commutative Absorption 2
                  q :||: (p :&&: q),                                        -- 12. Commutative Absorption 2   
                  Not p :||: (q :&&: Not p),                                -- 13. Commutative Absorption 2  
                  p :&&: (q :||: p),                                        -- 14. Commutative Absorption 3
                  Not (Not q) :&&: (Not (Not p) :||: Not( Not q)),          -- 15. Commutative Absorption 3
                  (p :&&: q) :&&: (p :||: (p :&&: q)),                      -- 16. Commutative Absorption 3
                  p :||: (p :&&: q),                                        -- 17. Commutative Absorption 4
                  Not (Not p) :||: (Not (Not p) :&&: q),                    -- 18. Commutative Absorption 4
                  (p :&&: q) :||: ((p :&&: q) :&&: q),                      -- 19. Commutative Absorption 4
                  (q :||: p) :&&: p,                                        -- 20. Commutative Absorption 5
                  (p :||: (p :&&: q)) :&&: (p :&&: q),                      -- 21. Commutative Absorption 5                  
                  (Not (Not p) :||: Not (Not q)) :&&: Not (Not q),          -- 22. Commutative Absorption 5
                  ((p :&&: q) :||: q) :&&: ((p :&&: q) :||: q),             -- 23. Absorption, somewhere, repeat
                  ((p :||: q) :&&: p) :&&: ((p :&&: q) :||: r),             -- 24. Commutative Absorption 2, somewhere, repeat
                  ((p :||: q) :&&: p) :&&: ((p :&&: q) :||: q),             -- 25. Commutative Absorption 2, somewhere, repeat
                  (((p :||: q) :&&: q) :&&: ((p :&&: q) :||: q)) :||: q,    -- 26. Commutative Absorption 4, Absorption, somewhere, repeat
                  (p :&&: (p :||: q)) :||: ((p :&&: q) :||: p) :||: ((p :||: q) :&&: p) :||: (p :&&: (q :||: p)) :||: ((p :&&: q) :||: ((p :&&: q) :&&: q)) :||: ((q :||: p) :&&: p)
                                                                            -- 26. Commutative All Absorption variants, somewhere, repeat
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
                 p :&&: F,                             -- 0. F-rule Conjunction
                 (p :||: q) :&&: F,                    -- 1. F-rule Conjunction
                 p :&&: T,                             -- 2. T-rule Conjunction
                 F :&&: p,                             -- 3. Commutative F-rule Conjunction
                 F :&&: (p :||: q),                    -- 4. Commutative F-rule Conjunction
                 T :&&: p,                             -- 5. Commutative T-rule Conjunction
                 (F :&&: p) :&&: p,                    -- 6. Somewhere, commutative F-rule Conjunction 
                 p :&&: (F :&&: p),                    -- 7. Somewhere, commutative F-rule Conjunction
                 (T :&&: p) :&&: p,                    -- 8. Somewhere, commutative T-rule Conjunction                 
                 p :&&: (T :&&: p),                    -- 9. Somewhere commutative T-rule Conjunction
                 (F :&&: p) :&&: (p :&&: F) :&&: p,    -- 10. Somewhere, repeat, F-rule Conjunction and commutative F-rule Conjunction
                 (F :&&: p) :&&: (p :&&: F),           -- 11. Somewhere, repeat, F-rule Conjunction and commutative F-rule Conjunction 
                 (T :&&: p) :&&: (p :&&: T) :&&: p,    -- 12. Somewhere, repeat, T-rule Conjunction and commutative T-rule Conjunction
                 (T :&&: p) :&&: (p :&&: T)            -- 13. Somewhere, repeat, T-rule Conjunction and commutative T-rule Conjunction
                ]

boolRuleDisjunctionTestSet =
                [
                 p :||: F,                             -- 0. F-rule disjunction
                 p :||: T,                             -- 1. T-rule disjunction
                 (p :&&: q) :||: T,                    -- 2. T-rule disjunction
                 F :||: p,                             -- 3. Commutative F-rule disjunction 
                 T :||: p,                             -- 4. Commutative T-rule disjunction
                 T :||: (p :&&: q),                    -- 5. Commutative T-rule disjunction
                 (F :||: p) :||: (p :||: F),           -- 6. Somewhere, repeat, F-rule disjunction and commutative F-rule disjunction
                 (T :||: p) :||: (p :||: T),           -- 7. Somewhere, repeat, T-rule disjunction and commutative T-rule disjunction
                 (F :||: p) :||: p,                    -- 8. Somewhere, commutative f-rule disjunction 
                 (T :||: p) :||: p,                    -- 9. Somewhere, commutative T-rule disjunction 
                 p :&&: (F :||: p),                    -- 10. Somewehere, commutative F-rule disjunction
                 p :||: (T :||: p),                    -- 11. Somewhere, commutative T-rule disjunction
                 (F :||: p) :||: (p :||: F) :||: p,    -- 12. Somewhere, repeat, F-rule disjunction and commutative F-rule disjunction
                 (T :||: p) :||: (p :||: T) :||: p     -- 13. Somewhere, repeat, T-rule disjunction and commutative T-rule disjunction
                ]

boolRuleComplementTestSet =
                [
                 p :&&: Not p,                        -- 0. Complement, F-rule
                 (p :&&: q) :&&: Not (p :&&: q),      -- 1. Complement, F-rule
                 p :||: Not p,                        -- 2. Complement, T-rule
                 (p :||: q) :||: Not (p :||: q),      -- 3. Complement, T-rule
                 Not p :&&: p,                        -- 4. Commutative complement, F-rule
                 Not p :||: p,                        -- 5. Commutative complement, T-rule
                 (Not p :&&: p) :&&: T,               -- 6. Somewhere, commutative complement, F-rule
                 (Not p :||: p) :||: F                -- 7. Somewhere, commutative complement, T-rule
                ]

boolRuleNotTestSet =
                [
                 Not T,                               -- 0. F-rule Not T
                 Not F,                               -- 1. T-rule Not F
                 p :&&: Not T,                        -- 2. Somewhere, F-rule Not T
                 p :&&: Not F,                        -- 3. Somewhere, T-rule Not F
                 p :&&: Not (Not T),                  -- 4. Somewhere, repeat, F-rule Not T
                 p :&&: Not (Not F),                  -- 5. Somewhere, repeat, T-rule Not F
                 p :&&: Not T :||: Not T,             -- 6. Somewhere, repeat, F-rule Not T
                 p :&&: Not F :||: Not F              -- 7. Somewhere, repeat, T-rule Not F
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
                  (q :&&: p) :&&: r,                                   -- (q ˄ p) ˄ r
                  Not ((q :&&: p) :&&: r),                             -- ¬((q ˄ p) ˄ r)  
                  (q :||: p) :||: r,                                   -- (q ˅ p) ˅ r
                  Not ((q :||: p) :||: r),                             -- ¬((q ˅ p) ˅ r)  
                  ((q :||: p) :||: r) :||: s,                          -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((q :||: p) :||: r) :||: s),                    -- ¬((q ˅ p) ˅ r) ˅ s)  
                  ((q :||: p) :||: r) :||: s,                          -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((q :||: p) :||: r) :||: s),                    -- ¬((q ˅ p) ˅ r) ˅ s)  
                  (Not (Not p) :&&: T) :&&: T,                         -- (¬¬p ˄ T) ˄ T
                  Not ((Not (Not p) :&&: T) :&&: F),                   -- ¬((¬¬p ˄ T) ˄ F)                
                  Not ( T :&&: (Not (Not p) :&&: T) :&&: Not (Not p)), -- ¬(T ˄ (¬¬p ˄ T) ˄ ¬¬p)    
                  Not ((Not (Not p) :||: T) :||: T :||: Not (Not q))   -- ¬((¬¬p ˄ T) ˄ T ˄ ¬¬q)        
                ]

distributivityTestSet =
                [ 
                  p :||: (q :||: r),                    -- No derivation - p ˅ (q ˅ r)
                  p :||: (q :&&: r),                    -- Left distributivity - p ˅ (q ˄ r)
                  p :&&: (q :||: r),                    -- Left distributivity - p ˄ (q ˅ r)
                  o :&&: ((p :&&: q) :||: r),           -- Left distributivity - o ˄ ((p ˄ q) ˅ r)
                  o :||: ((p :&&: q) :||: r),           -- Left distributivity - o ˅ ((p ˄ q) ˅ r)
                  o :||: (((p :&&: q) :||: r) :&&: s),  -- Left distributivity - o ˅ ((p ˄ q) ˅ r) ˄ s
                  o :&&: (((p :&&: q) :||: r) :||: s),  -- Left distributivity - o ˄ ((p ˄ q) ˅ r) ˅ s
                  o :||: (((p :&&: q) :||: r) :||: s),  -- Left distributivity - o ˅ ((p ˄ q) ˅ r) ˅ s
                  (p :&&: q) :||: r,                    -- Right distributivity - (p ˄ q) ˅ r)
                  (p :||: q) :&&: r,                    -- Right distributivity - (p ˅ q) ˄ r)
                  ((p :&&: q) :||: r) :||: s,           -- Right distributivity - ((p ˄ q) ˅ r) ˅ s
                  Not (p :||: (q :&&: r)),              -- Somewhere, left distributivity - ¬(p ˅ (q ˄ r))
                  Not ((p :&&: q) :||: r)               -- Somewhere, right distributivity - ¬((p ˄ q) ˅ r)
                ]

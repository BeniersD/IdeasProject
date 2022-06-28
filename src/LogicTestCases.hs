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
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p)                        -- ¬(¬¬p ˄ ¬p ˄ T)
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
                  (p :->: Not (Not p)) :<->: (Not p :<->: Not p)                        -- ¬(¬¬p ˄ ¬p ˄ T)
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
                  ((p :||: q) :&&: p) :&&: ((p :&&: q) :||: q),             -- 24. Commutative Absorption 1, somewhere, repeat
                  (((p :||: q) :&&: q) :&&: ((p :&&: q) :||: q)) :||: q     -- 25. Commutative Absorption 4, Absorption, somewhere, repeat
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
                 p :&&: F,                             -- 0. Conjunction
                 (p :||: q) :&&: F,                    -- 1. Conjunction
                 p :&&: T,                             -- 2. No derivation
                 F :&&: p,                             -- 3. Commutative conjunction
                 F :&&: (p :||: q),                    -- 4. Commutative conjunction
                 T :&&: p,                             -- 5. No derivation
                 (F :&&: p) :&&: (p :&&: F),           -- 6. Somewhere, repeat, conjunction and commutative conjunction 
                 (T :&&: p) :&&: (p :&&: T),           -- 7. No derivation
                 (F :&&: p) :&&: p,                    -- 8. Somewhere, commutative conjunction 
                 (T :&&: p) :&&: p,                    -- 9. No derivation
                 p :&&: (F :&&: p),                    -- 10. Somewhere, commutative conjunction
                 p :&&: (T :&&: p),                    -- 11. No derivation
                 (F :&&: p) :&&: (p :&&: F) :&&: p,    -- 12. Somewhere, repeat, conjunction and commutative conjunction
                 (T :&&: p) :&&: (p :&&: T) :&&: p     -- 13. No derivation
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
                  p :||: (q :&&: r),                    -- p ˅ (q ˄ r)
                  (p :&&: q) :||: r,                    -- (p ˄ q) ˅ r)
                  Not (p :||: (q :&&: r)),              -- ¬(p ˅ (q ˄ r))
                  Not ((p :&&: q) :||: r),              -- ¬((p ˄ q) ˅ r)
                  ((p :&&: q) :||: r) :||: s,           -- ((p ˄ q) ˅ r) ˅ s
                  o :&&: ((p :&&: q) :||: r),           -- o ˄ ((p ˄ q) ˅ r)
                  o :||: ((p :&&: q) :||: r),           -- o ˅ ((p ˄ q) ˅ r)
                  o :||: (((p :&&: q) :||: r) :&&: s),  -- o ˅ ((p ˄ q) ˅ r) ˄ s
                  o :&&: (((p :&&: q) :||: r) :||: s),  -- o ˄ ((p ˄ q) ˅ r) ˅ s
                  o :||: (((p :&&: q) :||: r) :||: s)   -- o ˅ ((p ˄ q) ˅ r) ˅ s
                ]

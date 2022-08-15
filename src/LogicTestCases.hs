module LogicTestCases 
     where

import Domain.Logic.Formula
import LogicFunctions

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Test sets to evaluate rules and/or strategies
-------------------------------------------------------------------------------------------------------------------------------------------------
commutativityTestSet, doubleNotTestSet, deMorganAndTestSetSimple, deMorganOrTestSetSimple, deMorganOrTestSetComplex, deMorganAndTestSetComplex, 
  deMorganAndDoubleNotTestSet,   implicationEliminationTestSet, implicationEliminationDerivTestSet , absorptionTestSet, idempotencyTestSet, 
  boolRuleConjunctionTestSet, boolRuleDisjunctionTestSet, boolRuleComplementTestSet, boolRuleNotTestSet, deMorganAndImplicationEliminationTestSet, 
  deMorganAndEquivalenceEliminationTestSet, equivalenceEliminationTestSet, deMorganDerivTestSet, equivalenceEliminationDerivTestSet, 
  layerTestSet, negationsTestSet :: [SLogic]
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
                  p :&&: Not (Not p),              -- 9.
                  Not (Not p :&&: p),              -- 10.
                  T :&&: F,                        -- 11. 
                  F :&&: T,                        -- 12.
                  T :&&: p,                        -- 13.  
                  p :&&: T,                        -- 14.
                  F :&&: p,                        -- 15.  
                  p :&&: F,                        -- 16.
                  p :&&: (q :&&: r),               -- 17.
                  p :||: (q :||: r),               -- 18.
                  p :&&: (q :&&: p),               -- 19.
                  T :&&: (p :||: q),               -- 20.
                  (p :&&: q) :&&: F,               -- 21.
                  (p :&&: q) :&&: (q :&&: p),      -- 22.
                  p :||: q,                        -- 23.
                  q :||: p,                        -- 24.
                  p :||: Not (Not p),              -- 25.
                  Not (Not T :||: p),              -- 26.
                  T :||: F,                        -- 27.
                  F :||: T,                        -- 28.
                  Not (Not (Not T)) :||: T,        -- 29.
                  Not (Not  F) :||: Not (Not (Not (Not T))), -- 30.
                  T :||: p,                        -- 31.
                  p :||: T,                        -- 32.
                  F :||: p,                        -- 33.
                  p :||: F,                        -- 34.
                  p :||: (Not p :||: q),               -- 35.
                  (p :||: q) :||: Not p,               -- 36.
                  (Not (Not p) :||: Not q) :||: (q :||: p),       -- 37.
                  (q :||: p) :||: (q :||: p),       -- 38.
                  (q :||: Not (Not p)) :||: (q :||: Not p),       -- 39.
                  (q :||: p) :||: (Not (Not q) :||: Not p)  :||: (Not q :||: Not (Not p)),       -- 40.
                  p :||: F :||: Not p :||: T :||: q :||: Not q :||: (p :&&: q) :||: (Not p :&&: q) :||: (p :&&: Not q) :||: (Not p :&&: Not q) :||: (p :&&: q :&&: r), -- 41.
                  (q :||: p) :||: (q :||: p) :||: (Not q :||: Not p) :||: (F :||: T),      -- 42.
                  q :||: p :||: q :||: p :||: Not q :||: Not p :||: F :||: T,  -- 43.
                  (q :||: p) :||: (q :||: p) :&&: ((q :->: p) :||: (q :<->: p) :||: (q :||: p) :||: p), -- 44.
                  Not p :&&: Not q :&&: Not r :&&: Not (Not q) :&&: Not (Not r) :&&: Not (Not q), --45
                  (Not p :&&: r :&&: Not (Not (Not F))) :||: Not (Not (Not F)) :||: Not (Not (Not r)) :||: Not (Not (Not p)) :||: (Not p :&&: q) :||: (Not (Not p) :&&: q :&&: Not p) :||: q :||: p :||: q :||: p :||: Not q :||: Not p :||: F :||: T :||: Not (Not (Not T)) :||: (Not q :<->: Not p)-- 46.
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
                  (T :->: F) :->: (F :->: T),
                  (p :->: q :->: r :->: s),
                  (p :->: Not (Not (Not q)) :->: Not (Not r) :->: Not s),
                  (p :->: Not (Not (Not q)) :->: Not (Not p) :->: Not q),
                  (T :->: Not (Not (Not q)) :->: Not (Not r) :->: F),
                  T :->: F :->: F :->: T,
                  (T :->: F :->: F :->: T)
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
                  Not (p :&&: q),                               -- 0. ¬(p ˄ q)  
                  Not (q :&&: p),                               -- 1. ¬(q ˄ p)  
                  Not (q :&&: p :&&: r),                        -- 2. ¬(q ˄ p ˄ r)  
                  Not (q :&&: p :&&: r :&&: s),                 -- 3. ¬(q ˄ p ˄ r ˄ s)  
                  Not (Not (q :&&: p)),                         -- 4. ¬¬(q ˄ p)  
                  Not (Not (Not p) :||: Not (Not q)),           -- 5. ¬(¬¬p ˅ ¬¬p)
                  Not (Not (Not p) :||: Not (Not (Not q))),     -- 6. ¬(¬¬p ˅ ¬¬¬q)
                  Not (Not (Not p) :||: Not (Not (Not q))) :->: Not (Not (Not p) :||: Not (Not (Not q)) :||: Not (Not r)),     -- 6. ¬(¬¬p ˅ ¬¬¬q) -> ¬(¬¬p ˅ ¬¬¬q ˅ ¬¬r) 
                  Not (Not (Not p) :||: Not (Not (Not q)) :||: Not (Not r) :||: Not (Not (Not (Not s)))),     -- 7.  ¬(¬¬p ˅ ¬¬¬q ˅ ¬¬r ˅ ¬¬¬¬s)
                  Not (Not (Not p) :&&: T),                     -- 7. ¬(¬¬p ˄ T) 
                  Not (Not (Not p) :&&: T :&&: T),              -- 8. ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not p) :&&: T :&&: F),              -- 9. ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not p) :&&: T :&&: Not (Not p)),    -- 10. ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not p) :&&: T :&&: Not (Not q)),    -- 11. ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not p) :&&: Not p :&&: T),          -- 12. ¬(¬¬p ˄ ¬p ˄ T)
                  Not (T :||: Not (Not p)),                     -- 13. ¬(T ˅ ¬¬p)
                  Not (q :||: p :||: r),                        -- 14. ¬(q ˅ p ˅ r)  
                  Not (q :||: p :||: r :||: s),                 -- 15. ¬(q ˅ p ˅ r ˅ s)                  
                  Not (Not (Not p) :||: T),                     -- 16. ¬(¬¬p ˅ T)
                  Not (Not (Not p) :||: T :||: T),              -- 17. ¬(¬¬p ˅ T ˅ T)        
                  Not (Not (Not p) :||: T :||: F),              -- 18. ¬(¬¬p ˅ T ˅ F)                 
                  Not (T :||: Not (Not p) :||: Not (Not p)),    -- 19. ¬(T ˅ ¬¬p ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not p)),    -- 20. ¬(¬¬p ˅ T ˅ ¬¬p)    
                  Not (Not (Not p) :||: T :||: Not (Not q)),    -- 21. ¬(¬¬p ˅ T ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: T),          -- 22. ¬(¬¬p ˅ ¬p ˅ T)
                  Not (F :&&: Not (Not p)),                     -- 23. ¬(F ˄ ¬¬p)
                  Not (Not (Not p) :&&: F),                     -- 24. ¬(¬¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: F),              -- 25. ¬(¬¬p ˄ F ˄ F)        
                  Not (Not (Not p) :&&: F :&&: T),              -- 26. ¬(¬¬p ˄ F ˄ T)                
                  Not (Not (Not p) :&&: F :&&: Not (Not p)),    -- 27. ¬(¬¬p ˄ F ˄ ¬¬p)    
                  Not (Not (Not p) :&&: F :&&: Not (Not q)),    -- 28. ¬(¬¬p ˄ F ˄ ¬¬q)        
                  Not (F :&&: Not (Not p) :&&: Not p),          -- 29. ¬(F ˄ ¬¬p ˄ ¬p)        
                  Not (Not (Not p) :&&: Not p :&&: F),          -- 30. ¬(¬¬p ˄ ¬p ˄ F)
                  Not (Not (Not p) :&&: F :&&: Not p),          -- 31. ¬(¬¬p ˄ F ˄ ¬p)                
                  Not (F :||: Not (Not p)),                     -- 32. ¬(T ˅ ¬¬p)
                  Not (F :||: Not (Not p) :||: Not (Not p)),    -- 33. ¬(T ˅ ¬¬p ˅ ¬¬p)            
                  Not (Not (Not p) :||: F),                     -- 34. ¬(¬¬p ˅ F)
                  Not (Not (Not p) :||: F :||: F),              -- 35. ¬(¬¬p ˅ F ˅ F)        
                  Not (Not (Not p) :||: F :||: T),              -- 36. ¬(¬¬p ˅ F ˅ T)                
                  Not (Not (Not p) :||: F :||: Not (Not p)),    -- 37. ¬(¬¬p ˅ F ˅ ¬¬p)    
                  Not (Not (Not p) :||: F :||: Not (Not q)),    -- 38. ¬(¬¬p ˅ F ˅ ¬¬q)        
                  Not (Not (Not p) :||: Not p :||: F),          -- 39. ¬(¬¬p ˅ ¬p ˅ F)   
                  Not (Not (Not p) :||: (Not p :&&: F))         -- 40. ¬(¬¬p ˅ ¬p ˄ F)                    
                ]

doubleNotTestSet =
                [ Not p, 
                  Not (Not p),
                  Not (Not (p :&&: q)),
                  Not (Not p) :&&: Not( Not p),
                  Not( Not( Not( Not p))) :||: Not( Not( Not( Not p))),
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
                  Not (Not (Not p) :&&: Not p :&&: T :||: Not p) :<->: Not p,            -- ¬(¬¬p ˄ ¬p ˄ T ˅ ¬p) ↔ ¬p
                  Not (p :||: q :||: r :||: Not q :||: Not r :||: Not q)
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
                  (p :&&: (r :||: q)) :||: (p :&&: (p :||: r)) :||: (p :&&: (p :||: q)) :||: ((p :&&: q) :||: p) :||: ((p :||: q) :&&: p) :||: (p :&&: (q :||: p)) :||: ((p :&&: q) :||: ((p :&&: q) :&&: q)) :||: ((q :||: p) :&&: p),
                                                                            -- 26. Commutative All Absorption variants, somewhere, repeat
                  p :&&: (Not p :||: q),                                    -- 27. Absorption Derivative
                  p :||: (q :&&: Not p),                                    -- 28. Absorption Derivative
                  (Not p :||: q) :&&: p,                                    -- 29. Absorption Derivative
                  (q :&&: Not p) :||: p                                     -- 30. Absorption Derivative    
                ]

idempotencyTestSet =
                [
                 p :&&: p,
                 Not( Not p) :&&: p,
                 p :||: p,
                 p :||: (q :||: p),
                 p :||: (p :||: q),
                 p :&&: (q :&&: p),
                 p :&&: (p :&&: q),
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
                 F :||: (p :&&: q),                    -- 4. Commutative T-rule disjunction
                 T :||: p,                             -- 5. Commutative T-rule disjunction
                 T :||: (p :&&: q),                    -- 6. Commutative T-rule disjunction
                 (F :||: p) :||: (p :||: F),           -- 7. Somewhere, repeat, F-rule disjunction and commutative F-rule disjunction
                 (T :||: p) :||: (p :||: T),           -- 8. Somewhere, repeat, T-rule disjunction and commutative T-rule disjunction
                 (F :||: p) :||: p,                    -- 9. Somewhere, commutative f-rule disjunction 
                 (T :||: p) :||: p,                    -- 10. Somewhere, commutative T-rule disjunction 
                 p :&&: (F :||: p),                    -- 11. Somewehere, commutative F-rule disjunction
                 p :||: (T :||: p),                    -- 12. Somewhere, commutative T-rule disjunction
                 (F :||: p) :||: (p :||: F) :||: p,    -- 13. Somewhere, repeat, F-rule disjunction and commutative F-rule disjunction
                 (T :||: p) :||: (p :||: T) :||: p     -- 14. Somewhere, repeat, T-rule disjunction and commutative T-rule disjunction
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
                  ((q :||: p) :||: r) :&&: (s :||: t),                 -- ((q ˅ p) ˅ r) ˄ (s ˅ t)  
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

layerTestSet =
                [
                  Not (Not p),
                  Not (Not (Not p)),
                  Not (Not (Not (Not p))),
                  Not (Not (Not (Not (Not p)))),
                  Not (Not (Not (Not (Not (Not p))))),
                  Not F,
                  Not (Not F),
                  Not (Not (Not T)),
                  Not (Not (Not (Not F))),
                  Not (Not (Not (Not (Not T)))),
                  Not (Not p) :||: q,
                  Not (Not p) :&&: q,
                  Not (Not p) :->: q,
                  Not (Not p) :<->: q,
                  Not( Not( Not p)) :||: Not( Not( Not( Not p))),
                  Not( Not( Not( Not p))) :||: Not( Not( Not( Not p))),
                  Not (Not (Not (Not (Not (Not p))))) :->: Not (Not q),
                  (Not (Not (Not (Not (Not (Not p))))) :->: Not (Not q)) :&&: Not (Not q),
                  ((Not (Not (Not (Not (Not (Not p))))) :&&: Not (Not q)) :->: (Not (Not q)) :&&: Not (Not (Not (Not (Not (Not q)))))),
                  p :&&: Not F,
                  p :&&: Not (Not F),
                  Not T :&&: q,
                  Not (Not p) :&&: Not F,
                  Not (Not p) :&&: Not (Not F),
                  Not (Not T) :&&: Not F,
                  Not T :&&: Not F,
                  Not (Not F) :&&: Not (Not T),
                  Not (Not (Not F)) :&&: Not (Not (Not T)),
                  p :||: Not (Not q),
                  p :&&: Not (Not q),
                  Not (Not p) :&&: Not (Not q),
                  Not (Not p) :||: Not (Not q), 
                  Not (p :&&: Not p),
                  Not (Not p :&&: p),
                  Not (p :||: Not p),
                  Not (Not p :||: p),
                  Not (p :&&: F), 
                  Not (p :&&: T),     
                  Not (p :||: F),
                  Not (F :||: p), 
                  Not (T :||: p),
                  Not (p :&&: p),
                  Not (Not p :&&: Not p),
                  p :->: q,
                  Not p :->: q,
                  p :->: Not q,
                  Not p :->: Not q,
                  p :<->: q,
                  Not p :<->: q,
                  p :<->: Not q,
                  Not p :<->: Not q,
                  Not (p :&&: q),
                  Not (Not p :&&: q),
                  Not (p :&&: Not q),
                  Not (Not p :&&: Not q),
                  Not (p :||: q),
                  Not (Not p :||: q),
                  Not (p :||: Not q),
                  Not (Not p :||: Not q),
                  Not (T :->: F),
                  Not (p :->: q),
                  Not (p :->: Not p),
                  Not (p :->: T),
                  Not (T :<->: F),
                  Not (p :<->: q),
                  Not (p :<->: Not p),
                  Not (p :<->: T)
                ]

negationsTestSet = [              
                  Not (p :&&: q),
                  Not (Not p :&&: q),
                  Not (p :&&: Not q),
                  Not (Not p :&&: Not q),
                  Not (p :||: q),
                  Not (Not p :||: q),
                  Not (p :||: Not q),
                  Not (Not p :||: Not q),
                  Not (p :&&: p),
                  Not (Not p :&&: p),
                  Not (p :&&: Not p),
                  Not (Not p :&&: Not p),
                  Not (p :||: p),
                  Not (p :||: (q :||: p)),
                  Not (p :||: (p :||: q)),
                  Not (p :&&: (q :&&: p)),
                  Not (p :&&: (p :&&: q)),                  
                  Not (Not p :||: p),
                  Not (p :||: Not p),
                  Not (Not p :||: Not p),
                  Not (T :||: q),
                  Not (Not p :||: F),            
                  Not (T :||: Not F),
                  Not (F :||: T),
                  Not (F :||: Not p),
                  Not (T :&&: q),
                  Not (Not p :&&: F),            
                  Not (Not T :&&: F),
                  Not (F :&&: T),
                  Not (F :&&: Not p),
                  Not (p :&&: (Not p :||: q)),
                  Not (p :||: (q :&&: Not p)),
                  Not((Not p :||: q) :&&: p),
                  Not ((q :&&: Not p) :||: p),               
                  Not (T :->: F),
                  Not (p :->: q),
                  Not (p :->: Not p),
                  Not (p :->: T),
                  Not (T :<->: F),
                  Not (p :<->: q),
                  Not (p :<->: Not p),
                  Not (p :<->: T)
                ]

thesisTestSet = [
                  Not ( p :||: Not q),
                  (Not p :&&: q),
                  Not ( p :||: Not q) :&&: Not (Not (p :||: Not q) :&&: r),
                  (Not p :&&: q) :&&: Not (Not (p :||: Not q) :&&: r),
                  (Not p :&&: Not (Not q)) :&&: Not ((Not p :&&: Not (Not q)) :&&: r),
                  (Not p :&&: Not (Not q)) :&&: (Not (Not (p :||: Not q)) :||: Not r),
                  Not (p :||: Not q) :&&: ((Not (Not p) :||: Not (Not (Not q))) :||: Not r),
                  Not (p :->: Not q),
                  Not (Not p :||: Not q),
                  Not (Not p) :&&: Not (Not q),
                  Not (Not p) :&&: q,

                  Not (p :->: Not q) :->: Not (p :->: Not q),
                  (p :&&: q) :->: Not (p :->: Not q),
                  Not (p :->: Not q) :->: (p :&&: q),
                  (p :&&: q) :->: (p :&&: q),
                  (Not p :||: Not q) :||: Not (Not p :||: Not q),
                  Not (Not p :||: Not q) :->: Not (p :->: Not q),                  
                  Not (p :->: Not q) :->: Not (Not p :||: Not q),
                  (p :->: Not q) :||: Not (p :->: Not q),
                  Not (p :->: Not q) :->: Not (p :->: Not q),
                  Not (p :->: Not q) :->: (p :&&: q),                   
                  (Not p :||: Not q) :||: (p :&&: q),

                  Not (p :||: Not q) :&&: Not (Not (p :||: Not q) :&&: r),
                  (Not p :&&: q) :&&: Not (Not (p :||: Not q) :&&: r),
                  (Not p :&&: Not (Not q)) :&&: Not ((Not p :&&: Not (Not q)) :&&: r),
                  (Not p :&&: Not (Not q)) :&&: (Not (Not (p :||: Not q)) :||: Not r),
                  Not (p :||: Not q) :&&: ((Not (Not p) :||: Not (Not (Not q))) :||: Not r)
                ]
module LogicTestCases
     where

import LogicReductionRules
import Domain.Logic.Formula  hiding (not)

type LgcChar  = Logic Char
type LsLgcChar = [LgcChar]

doubleNotTestSet, deMorganAndTestSetSimple, deMorganOrTestSetSimple, deMorganOrTestSetComplex, deMorganAndTestSetComplex, deMorganAndDoubleNotTestSet,
  implicationEliminationTestSet, implicationEliminationDerivTestSet , absorptionTestSet, idempotencyTestSet, boolRuleConjunctionTestSet, boolRuleDisjunctionTestSet,
  boolRuleComplementTestSet, boolRuleNotTestSet, deMorganAndImplicationEliminationTestSet, deMorganAndEquivalenceEliminationTestSet,
  equivalenceEliminationTestSet, deMorganDerivTestSet, equivalenceEliminationDerivTestSet, commutativityTestSet :: LsLgcChar
commutativityTestSet =
                [ Var 'p' :&&: Var 'q',
                  Var 'q' :&&: Var 'p',
                  Var 'p' :&&: Not (Var 'p'),
                  Var 'q' :&&: Not (Var 'p'),
                  Var 'p' :&&: Not (Var 'q'),
                  Var 'p' :&&: Not (Not (Var 'p')),
                  Not (Not (Var 'p') :&&: Var 'p'),
                  T :&&: F,
                  F :&&: T,
                  T :&&: Var 'p',
                  Var 'p' :&&: T,
                  F :&&: Var 'p',
                  Var 'p' :&&: F, 
                  Var 'p' :&&: (Var 'p' :&&: Var 'q'),
                  (Var 'p' :&&: Var 'q') :&&: Var 'p',
                  (Var 'p' :&&: Var 'q') :&&: (Var 'q' :&&: Var 'p'),
                  Var 'p' :||: Var 'q',
                  Var 'q' :||: Var 'p',
                  Var 'p' :||: Not (Not (Var 'p')),
                  Not (Not (Var 'p') :||: Var 'p'),
                  T :||: F,
                  F :||: T,
                  T :||: Var 'p',
                  Var 'p' :||: T,
                  F :||: Var 'p',
                  Var 'p' :||: F, 
                  Var 'p' :||: (Var 'p' :||: Var 'q'),
                  (Var 'p' :||: Var 'q') :||: Var 'p',
                  (Var 'p' :||: Var 'q') :||: (Var 'q' :||: Var 'p')
                ]

implicationEliminationDerivTestSet =
                [ Var 'p' :->: Var 'q',
                  Var 'p' :->: Not (Not (Var 'p')),
                  Var 'p' :->: (Var 'p' :&&: Var 'q'),
                  T :->: F,
                  F :->: T,
                  T :->: Var 'p',
                  F :->: Var 'p',
                  Var 'p' :->: T,
                  Var 'p' :->: F,
                  T :->: Not (Not (Var 'p')),
                  Not (Not (Var 'p')) :->: T,
                  (Var 'p' :&&: Var 'q') :->: F,
                  (Var 'p' :||: Var 'q') :->: T,              
                  (Var 'p' :->: T) :&&: (F :->: Var 'p'),
                  (T :->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :->: F),
                  (Not (Not (Var 'p')) :->: T) :&&: (Not (Var 'p') :->: T),
                  (T :->: F) :->: (F :->: T)
                ]

equivalenceEliminationDerivTestSet = 
                [
                 Not (Var 'p' :<->: Var 'q'), 
                 Not (Var 'p' :<->: Not (Not (Var 'q'))),
                 Var 'p' :<->: Not (Not (Var 'p')),
                 Not (F :<->: T),
                 Not (T :<->: F),
                 Not (F :<->: Var 'q'),
                 Not (Not (Not (Var 'q'))) :<->: T,
                 (Var 'p' :<->: T) :&&: (F :<->: Var 'p'),
                 (F :<->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :<->: T),
                 (Not (Not (Var 'p')) :<->: Not (Not (Var 'p'))) :&&: (F :<->: Not (Var 'p')),
                 (Var 'p' :<->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: T)
                ]

deMorganDerivTestSet =
                [ 
                  Not (Var 'p' :&&: Var 'q'),                                   -- ¬(p ˄ q)  
                  Not (Var 'q' :&&: Var 'p'),                                   -- ¬(q ˄ p)  
                  Not (Var 'q' :&&: Var 'p' :&&: Var 'r'),                      -- ¬(q ˄ p ˄ r)  
                  Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's'),         -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (Not (Var 'q' :&&: Var 'p')),                             -- ¬¬(q ˄ p)  
                  Not (Not (Not (Var 'p')) :&&: T),                             -- ¬(¬¬p ˄ T) 
                  Not (Not (Not (Var 'p')) :&&: T :&&: T),                      -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not (Var 'p')) :&&: T :&&: F),                      -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'p'))),    -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'q'))),    -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not (Var 'p')) :&&: Not (Var 'p') :&&: T),          -- ¬(¬¬p ˄ ¬p ˄ T)
                  Not (T :||: Not (Not (Var 'p'))),                             -- ¬(T ˅ ¬¬p)
                  Not (Var 'q' :||: Var 'p' :||: Var 'r'),                      -- ¬(q ˅ p ˅ r)  
                  Not (Var 'q' :||: Var 'p' :||: Var 'r' :||: Var 's'),         -- ¬(q ˅ p ˅ r ˅ s)                  
                  Not (Not (Not (Var 'p')) :||: T),                             -- ¬(¬¬p ˅ T)
                  Not (Not (Not (Var 'p')) :||: T :||: T),                      -- ¬(¬¬p ˅ T ˅ T)        
                  Not (Not (Not (Var 'p')) :||: T :||: F),                      -- ¬(¬¬p ˅ T ˅ F)                
                  Not (T :||: Not (Not (Var 'p')) :||: Not (Not (Var 'p'))),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)    
                  Not (Not (Not (Var 'p')) :||: T :||: Not (Not (Var 'p'))),    -- ¬(¬¬p ˅ T ˅ ¬¬p)    
                  Not (Not (Not (Var 'p')) :||: T :||: Not (Not (Var 'q'))),    -- ¬(¬¬p ˅ T ˅ ¬¬q)        
                  Not (Not (Not (Var 'p')) :||: Not (Var 'p') :||: T),          -- ¬(¬¬p ˅ ¬p ˅ T)
                  Not (F :&&: Not (Not (Var 'p'))),                             -- ¬(T ˄ ¬¬p)
                  Not (Not (Not (Var 'p')) :&&: F),                             -- ¬(¬¬p ˄ F)
                  Not (Not (Not (Var 'p')) :&&: F :&&: F),                      -- ¬(¬¬p ˄ F ˄ F)        
                  Not (Not (Not (Var 'p')) :&&: F :&&: T),                      -- ¬(¬¬p ˄ F ˄ T)                
                  Not (Not (Not (Var 'p')) :&&: F :&&: Not (Not (Var 'p'))),    -- ¬(¬¬p ˄ F ˄ ¬¬p)    
                  Not (Not (Not (Var 'p')) :&&: F :&&: Not (Not (Var 'q'))),    -- ¬(¬¬p ˄ F ˄ ¬¬q)        
                  Not (F :&&: Not (Not (Var 'p')) :&&: Not (Var 'p')),          -- ¬(F ˄ ¬¬p ˄ ¬p)        
                  Not (Not (Not (Var 'p')) :&&: Not (Var 'p') :&&: F),          -- ¬(¬¬p ˄ ¬p ˄ F)
                  Not (Not (Not (Var 'p')) :&&: F :&&: Not (Var 'p')),          -- ¬(¬¬p ˄ F ˄ ¬p)                
                  Not (F :||: Not (Not (Var 'p'))),                             -- ¬(T ˅ ¬¬p)
                  Not (F :||: Not (Not (Var 'p')) :||: Not (Not (Var 'p'))),    -- ¬(T ˅ ¬¬p ˅ ¬¬p)            
                  Not (Not (Not (Var 'p')) :||: F),                             -- ¬(¬¬p ˅ F)
                  Not (Not (Not (Var 'p')) :||: F :||: F),                      -- ¬(¬¬p ˅ F ˅ F)        
                  Not (Not (Not (Var 'p')) :||: F :||: T),                      -- ¬(¬¬p ˅ F ˅ T)                
                  Not (Not (Not (Var 'p')) :||: F :||: Not (Not (Var 'p'))),    -- ¬(¬¬p ˅ F ˅ ¬¬p)    
                  Not (Not (Not (Var 'p')) :||: F :||: Not (Not (Var 'q'))),    -- ¬(¬¬p ˅ F ˅ ¬¬q)        
                  Not (Not (Not (Var 'p')) :||: Not (Var 'p') :||: F)           -- ¬(¬¬p ˅ ¬p ˅ F)
                ]

doubleNotTestSet =
                [ Not (Var 'p'), 
                  Not (Not (Var 'p')),
                  Not (Not (Var 'p')) :&&: Not( Not( Var 'p')),
                  Not (Not (Var 'p')) :&&: Not( Not( Var 'p')) :||: Not( Not( Not( Not( Var 'p')))),
                  Not (Not (Var 'p')) :||: Not( Not( Var 'p')) :||: Not( Not( Not( Not( Var 'p')))),
                  Not (Not (Var 'p')) :&&: Not( Not( Var 'p')) :&&: Not( Not( Not( Not( Var 'p')))) :||: Not( Not( Not( Not( Var 'p')))),
                  Not (Not (Var 'p')) :||: Not( Not( Var 'p')) :||: Not( Not( Not( Not( Var 'p')))) :||: Not( Not( Not( Not( Var 'p')))),
                  Not (Not (Not (Not (Not (Not (Var 'p')) :||: Not( Not( Var 'p')))))),
                  Not (Not (Not (Not (Var 'p')) :||: Not (Not (Var 'p')) :||: Not (Not (Not (Not (Var 'p')))))),
                  (Var 'p' :->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: Not (Var 'p')),
                  (Var 'p' :->: Not (Not (Var 'p')) :<->: Not (Not (Var 'p'))) :<->: (Not (Var 'p'):<->: Not (Var 'p')),
                  (Var 'p' :->: Not (Not (Not (Var 'p')):<->: Not (Not (Var 'p')))) :<->: (Not (Var 'p'):<->: Not (Not (Var 'p')))
                ]

deMorganAndTestSetSimple = 
                [ Not (Var 'p' :&&: Var 'q'),                                                                              -- ¬(q ˄ p)  
                  Not (Var 'q' :&&: Var 'p' :&&: Var 'r'),                                                                 -- ¬(q ˄ p ˄ r)  
                  Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's'),                                                    -- ¬(q ˄ p ˄ r ˄ s)  
                  Not (Var 'q' :&&: Var 'p' :&&: Var 'r' :&&: Var 's' :&&: Var 't')                                       -- ¬(q ˄ p ˄ r ˄ s ˄ t)                  
                ]

deMorganOrTestSetSimple =                   
                [ Not (Var 'p' :||: Var 'q'),                                                                              -- ¬(q ˅ p)  
                  Not (Var 'q' :||: Var 'p' :||: Var 'r'),                                                                 -- ¬(q ˅ p ˅ r)  
                  Not (Var 'q' :||: Var 'p' :||: Var 'r' :||: Var 's'),                                                    -- ¬(q ˅ p ˅ r ˅ s)  
                  Not (Var 'q' :||: Var 'p' :||: Var 'r' :||: Var 's' :||: Var 't')                                        -- ¬(q ˅ p ˅ r ˅ s ˅ t)                  
                ]

deMorganAndTestSetComplex = 
                [ 
                  Not (Not (Var 'p' :&&: Var 'q')),                                                                          -- ¬¬(p ˄ q)  
                  Not (Not (Var 'q' :&&: Var 'p')),                                                                          -- ¬¬(q ˄ p)  
                  Not (Not (Var 'q' :&&: Var 'p' :&&: Var 'r')),                                                             -- ¬¬(q ˄ p ˄ r)  
                  Not (Var 'q' :||: Var 'p' :&&: Var 'r'),                                                                   -- ¬¬(q ˄ p ˄ r)  
                  Not (Var 'q' :||: Var 'p' :&&: Var 'r' :||: Var 's'),                                                      -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (Var 'q' :||: Var 'p' :||: Var 's' :&&: Var 'r'),                                                      -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (Var 'p' :->: Not (Not (Var 'p') :<->: Not (Not (Var 'p')))) :<->: Not (Not (Var 'p') :&&: Not (Var 'p')), -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q'),                                                -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (Var 'p' :&&: Var 'q') :||: Not( Var 'p' :&&: Var 'q')),                                          -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (Var 'p' :&&: Var 'q')) :||: Not (Not (Var 'p' :&&: Var 'q')),                                    -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q')),                                          -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q')) :||: Var 'p'),                       -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not (Var 'p')) :&&: T),                                                                          -- ¬(¬¬p ˄ T) 
                  Not (Not (Not (Var 'p')) :&&: T :&&: T),                                                                   -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not (Var 'p')) :&&: T :&&: F),                                                                   -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'p'))),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not (Var 'p')) :&&: T :&&: Not (Not (Var 'q'))),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not (Var 'p')) :&&: Not (Var 'p') :&&: T),                                                       -- ¬(¬¬p ˄ ¬p ˄ T)
                  (Var 'p' :->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: Not (Var 'p'))                               -- ¬(¬¬p ˄ ¬p ˄ T)
                ]

deMorganOrTestSetComplex = 
                [ 
                  Not (Not (Var 'p' :||: Var 'q')),                                                                          -- ¬¬(p ˄ q)  
                  Not (Not (Var 'q' :||: Var 'p')),                                                                          -- ¬¬(q ˄ p)  
                  Not (Not (Var 'q' :||: Var 'p' :||: Var 'r')),                                                             -- ¬¬(q ˄ p ˄ r)  
                  Not (Var 'q' :||: Var 'p' :&&: Var 'r'),                                                                   -- ¬¬(q ˄ p ˄ r)  
                  Not (Var 'q' :||: Var 'p' :&&: Var 'r' :||: Var 's'),                                                      -- ¬¬(q ˄ p ˄ r ˄ s)  
                  Not (Var 'q' :||: Var 'p' :||: Var 's' :&&: Var 'r'),                                                      -- ¬¬(q ˄ p ˄ s ˄ r)  
                  (Var 'p' :->: Not (Not (Var 'p') :<->: Not (Not (Var 'p')))) :<->: Not (Not (Var 'p') :||: Not (Var 'p')), -- (p → ¬(¬p ↔ ¬¬p)) ↔ ¬(¬p ˄ p))                  
                  Not (Var 'p' :||: Var 'q') :||: Not (Var 'p' :||: Var 'q'),                                                -- ¬(p ˄ q) ˅ ¬(p ˄ q)
                  Not (Not (Var 'p' :||: Var 'q') :||: Not( Var 'p' :||: Var 'q')),                                          -- ¬(¬(p ˄ q) ˅ ¬(p ˄ q))                
                  Not (Not (Var 'p' :||: Var 'q')) :||: Not (Not (Var 'p' :||: Var 'q')),                                    -- ¬¬(p ˄ q) ˅ ¬¬(p ˄ q))
                  Not (Not (Var 'p' :||: Var 'q') :||: Not (Var 'p' :||: Var 'q')),                                          -- ¬¬(p ˄ q) ˅ ¬(p ˄ q))
                  Not (Not (Not (Var 'p' :||: Var 'q') :||: Not (Var 'p' :||: Var 'q')) :||: Var 'p'),                       -- ¬(¬(¬(p ˄ q) ˅ ¬(p ˄ q))) ˅ p)
                  Not (Not (Not (Var 'p')) :||: T),                                                                          -- ¬(¬¬p ˄ T) 
                  Not (Not (Not (Var 'p')) :||: T :||: T),                                                                   -- ¬(¬¬p ˄ T ˄ T)
                  Not (Not (Not (Var 'p')) :||: T :||: F),                                                                   -- ¬(¬¬p ˄ T ˄ F)                
                  Not (Not (Not (Var 'p')) :||: T :||: Not (Not (Var 'p'))),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬p)    
                  Not (Not (Not (Var 'p')) :||: T :||: Not (Not (Var 'q'))),                                                 -- ¬(¬¬p ˄ T ˄ ¬¬q)        
                  Not (Not (Not (Var 'p')) :||: Not (Var 'p') :&&: T),                                                       -- ¬(¬¬p ˄ ¬p ˄ T)
                  (Var 'p' :->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: Not (Var 'p'))                               -- ¬(¬¬p ˄ ¬p ˄ T)
                ]

deMorganAndDoubleNotTestSet =
                [ Var 'p',
                  Not (Var 'p'),
                  Not (Not (Var 'p')),
                  Not (Var 'p' :&&: Var 'q'), 
                  Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q'),
                  Not (Not (Var 'p' :&&: Var 'q') :||: Not( Var 'p' :&&: Var 'q')),
                  Not (Not (Var 'p' :&&: Var 'q')) :||: Not (Not (Var 'p' :&&: Var 'q')),
                  Not (Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q')),
                  Not (Not (Not (Var 'p' :&&: Var 'q') :||: Not (Var 'p' :&&: Var 'q')) :||: Var 'p'),
                  (Var 'p' :->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: Not (Var 'p')),
                  (Var 'p' :->: Not (Not (Var 'p') :<->: Not (Not (Var 'p')))) :<->: (Not (Var 'p') :<->: Not (Var 'p'))
                ]

implicationEliminationTestSet =
                [
                  Var 'p' :->: Not (Not (Var 'p')),
                  (Var 'p' :->: Var 'p') :&&: (Var 'p' :->: Var 'p'),
                  (Var 'p' :->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :->: Not (Var 'p')),
                  (Not (Not (Var 'p')) :->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :->: Not (Var 'p')),
                  (Var 'p' :->: Not (Not (Var 'p'))) :->: (Not (Var 'p') :->: Not (Var 'p'))
                ]

equivalenceEliminationTestSet =
                [
                 Var 'p' :<->: Not (Not (Var 'p')),
                 (Var 'p' :<->: Var 'p') :&&: (Var 'p' :<->: Var 'p'),
                 (Var 'p' :<->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :<->: Not (Var 'p')),
                 (Not (Not (Var 'p')) :<->: Not (Not (Var 'p'))) :&&: (Not (Var 'p') :<->: Not (Var 'p')),
                 (Var 'p' :<->: Not (Not (Var 'p'))) :<->: (Not (Var 'p') :<->: Not (Var 'p'))
                ]

absorptionTestSet =
                [
                 (Var 'p' :&&: Var 'q') :||: Var 'r',
                 Var 'p' :&&: Not( Var 'q') :||: Var 'r',
                 (Var 'p' :&&: Var 'q') :||: Var 'q',
                 (Var 'p' :&&: Not( Var 'q')) :||: Not( Var 'q'),
                 Var 'r' :||: (Var 'p' :&&: Var 'q'),
                 Not (Not (Var 'r')) :||: (Var 'p' :&&: Var 'q'),
                 Var 'p' :||: (Var 'p' :&&: Var 'q'),
                 Not (Not (Var 'p')) :||: (Var 'p' :&&: Var 'q'),
                 Var 'q' :||: (Var 'p' :&&: Var 'q'),
                 Var 'q' :||: (Var 'p' :&&: Not( Not( Var 'q'))),
                 Var 'p' :&&: (Var 'p' :||: Var 'q'),
                 Var 'p' :&&: (Var 'p' :||: Var 'q'),
                 Var 'q' :&&: (Var 'p' :||: Var 'q'),
                 Not (Not (Var 'q')) :&&: (Not (Not (Var 'p')) :||: Not( Not (Var 'q'))),
                 (Var 'p' :||: Var 'q') :&&: Var 'p',
                 (Var 'p' :||: Var 'q') :&&: Not (Not (Var 'p')),
                 (Var 'p' :||: Var 'q') :&&: Var 'q',
                 (Not (Not (Var 'p')) :||: Not (Not (Var 'q'))) :&&: Not (Not (Var 'q')),
                 ((Var 'p' :||: Var 'q') :&&: Var 'p') :&&: ((Var 'p' :&&: Var 'q') :||: Var 'q'),
                 ((Var 'p' :&&: Var 'q') :||: Var 'q') :&&: ((Var 'p' :&&: Var 'q') :||: Var 'q'),
                 (((Var 'p' :||: Var 'q') :&&: Var 'q') :&&: ((Var 'p' :&&: Var 'q') :||: Var 'q')) :||: Var 'q'
                ]

idempotencyTestSet =
                [
                 Var 'p' :&&: Var 'p',
                 Not( Not (Var 'p')) :&&: Var 'p',
                 Var 'p' :||: Var 'p',
                 Var 'p' :||: Not (Not (Var 'p')),
                 (Var 'p' :&&: Var 'q') :&&: (Var 'p' :&&: Var 'q'),
                 (Not (Not (Var 'p')) :&&: Var 'q') :&&: (Not (Not (Var 'p')) :&&: Not (Not (Var 'q'))),
                 (Var 'p' :||: Var 'p') :&&: (Var 'p' :||: Var 'p'),
                 (Var 'p' :||: Var 'p') :&&: (Var 'p' :||: Not (Not (Var 'p'))),
                 (Var 'p' :&&: Var 'p') :&&: (Var 'p' :||: Var 'p'),
                 (Var 'p' :&&: Not (Not (Var 'p'))) :&&: (Var 'p' :||: Not (Not (Var 'p'))),
                 (Var 'p' :&&: Var 'q') :&&: (Var 'q' :&&: Var 'p'),
                 (Var 'p' :&&: Not (Not (Var 'q'))) :&&: (Var 'q' :&&: Not (Not (Var 'p'))),
                 (Var 'p' :&&: Var 'p') :&&: Var 'p',
                 (Not (Not (Var 'p')) :&&: Not (Not (Var 'p'))) :&&: Not (Not (Var 'p'))
                ]

boolRuleConjunctionTestSet =
                [
                 Var 'p' :&&: F,
                 Var 'p' :&&: T,
                 F :&&: Var 'p',
                 T :&&: Var 'p',
                 (F :&&: Var 'p') :&&: (Var 'p' :&&: F),
                 (T :&&: Var 'p') :&&: (Var 'p' :&&: T),
                 (F :&&: Var 'p') :&&: Var 'p',
                 (T :&&: Var 'p') :&&: Var 'p',
                 Var 'p' :&&: (F :&&: Var 'p'), 
                 Var 'p' :&&: (T :&&: Var 'p'),
                 (F :&&: Var 'p') :&&: (Var 'p' :&&: F) :&&: Var 'p',
                 (T :&&: Var 'p') :&&: (Var 'p' :&&: T) :&&: Var 'p'
                ]

boolRuleDisjunctionTestSet =
                [
                 Var 'p' :||: F,
                 Var 'p' :||: T,
                 F :||: Var 'p',
                 T :||: Var 'p',
                 (F :||: Var 'p') :||: (Var 'p' :||: F),
                 (T :||: Var 'p') :||: (Var 'p' :||: T),
                 (F :||: Var 'p') :||: Var 'p',
                 (T :||: Var 'p') :||: Var 'p',
                 Var 'p' :&&: (F :||: Var 'p'),
                 Var 'p' :||: (T :||: Var 'p'), 
                 (F :||: Var 'p') :||: (Var 'p' :||: F) :||: Var 'p',
                 (T :||: Var 'p') :||: (Var 'p' :||: T) :||: Var 'p'
                ]

boolRuleComplementTestSet =
                [
                 Var 'p' :&&: Not (Var 'p'),
                 Var 'p' :||: Not( Var 'p'),
                 Not (Var 'p') :&&: Var 'p',
                 Not (Var 'p') :||: Var 'p',
                 (Not (Var 'p') :&&: Var 'p') :&&: T,
                 (Not (Var 'p') :||: Var 'p') :||: F
                ]

boolRuleNotTestSet =
                [
                 Not T,
                 Not F,
                 Var 'p' :&&: Not T,
                 Var 'p' :&&: Not F,
                 Var 'p' :&&: Not (Not T),
                 Var 'p' :&&: Not (Not F),
                 Var 'p' :&&: Not T :||: Not T,
                 Var 'p' :&&: Not F :||: Not F
                ]

deMorganAndImplicationEliminationTestSet =
                [
                 Not (Var 'p' :->: Var 'q'),
                 Not (Var 'p' :->: Not (Not (Var 'q'))),
                 Not (Var 'p' :->: Not (Not (Var 'q'))) :||: Not (Not (Var 'p') :->: Not (Not (Var 'q'))),
                 Not (Not (Var 'p' :->: Not (Not (Var 'q'))) :->: Not (Not (Var 'p') :->: Not (Not (Var 'q'))))
                ]

deMorganAndEquivalenceEliminationTestSet =
                [
                 Not (Var 'p' :<->: Var 'q'),
                 Not (Var 'p' :<->: Not (Not (Var 'q'))),
                 Not (Var 'p' :<->: Not (Not (Var 'q'))) :||: Not (Not (Var 'p') :<->: Not (Not (Var 'q'))),
                 Not (Not (Var 'p' :<->: Not (Not (Var 'q'))) :<->: Not (Not (Var 'p') :<->: Not (Not (Var 'q'))))
                ]

associativityTestSet =
                [ 
                  (Var 'q' :&&: Var 'p') :&&: Var 'r',                                 -- (q ˄ p) ˄ r
                  Not ((Var 'q' :&&: Var 'p') :&&: Var 'r'),                           -- ¬((q ˄ p) ˄ r)  
                  (Var 'q' :||: Var 'p') :||: Var 'r',                                 -- (q ˅ p) ˅ r
                  Not ((Var 'q' :||: Var 'p') :||: Var 'r'),                           -- ¬((q ˅ p) ˅ r)  
                  ((Var 'q' :||: Var 'p') :||: Var 'r') :||: Var 's',                  -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((Var 'q' :||: Var 'p') :||: Var 'r') :||: Var 's'),            -- ¬((q ˅ p) ˅ r) ˅ s)  
                  ((Var 'q' :||: Var 'p') :||: Var 'r') :||: Var 's',                  -- ((q ˅ p) ˅ r) ˅ s)  
                  Not (((Var 'q' :||: Var 'p') :||: Var 'r') :||: Var 's'),            -- ¬((q ˅ p) ˅ r) ˅ s)  
                  (Not (Not (Var 'p')) :&&: T) :&&: T,                                 -- (¬¬p ˄ T) ˄ T
                  Not ((Not (Not (Var 'p')) :&&: T) :&&: F),                           -- ¬((¬¬p ˄ T) ˄ F)                
                  Not ( T :&&: (Not (Not (Var 'p')) :&&: T) :&&: Not (Not (Var 'p'))), -- ¬(T ˄ (¬¬p ˄ T) ˄ ¬¬p)    
                  Not ((Not (Not (Var 'p')) :||: T) :||: T :||: Not (Not (Var 'q')))   -- ¬((¬¬p ˄ T) ˄ T ˄ ¬¬q)        
                ]

distributivityTestSet =
                [ 
                  Var 'p' :||: (Var 'q' :&&: Var 'r'),                                -- p ˅ (q ˄ r)
                  (Var 'p' :&&: Var 'q') :||: Var 'r',                                -- (p ˄ q) ˅ r)
                  Not (Var 'p' :||: (Var 'q' :&&: Var 'r')),                          -- ¬(p ˅ (q ˄ r))
                  Not ((Var 'p' :&&: Var 'q') :||: Var 'r'),                          -- ¬((p ˄ q) ˅ r)
                  ((Var 'p' :&&: Var 'q') :||: Var 'r') :||: Var 's',                 -- ((p ˄ q) ˅ r) ˅ s
                  Var 'o' :&&: ((Var 'p' :&&: Var 'q') :||: Var 'r'),                 -- o ˄ ((p ˄ q) ˅ r)
                  Var 'o' :||: ((Var 'p' :&&: Var 'q') :||: Var 'r'),                 -- o ˅ ((p ˄ q) ˅ r)
                  Var 'o' :||: (((Var 'p' :&&: Var 'q') :||: Var 'r') :&&: Var 's'),  -- o ˅ ((p ˄ q) ˅ r) ˄ s
                  Var 'o' :&&: (((Var 'p' :&&: Var 'q') :||: Var 'r') :||: Var 's'),  -- o ˄ ((p ˄ q) ˅ r) ˅ s
                  Var 'o' :||: (((Var 'p' :&&: Var 'q') :||: Var 'r') :||: Var 's')   -- o ˅ ((p ˄ q) ˅ r) ˅ s
                ]

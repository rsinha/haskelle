module Prover where

--General Approach:
--given: a goal
--step 1: check whether its an axion, if so we are done
--step 2: Use one of 6 inference rules to replace the goal by simpler subgoals. Recurse.

import List
import Haskelle

--rules

{-
Axiom	--------  (p atomic)
	p,A |- p
-}
axiomRule :: Sequent -> [([Sequent], Rule)]
axiomRule (Sequent assumptions [p])
	| (isAtomic p) && (List.elem p assumptions) = [([], Axiom)]
	| otherwise = []


{-
          p,q,A |- r
LeftConj  ----------
          p&q,A |- r
-}


{-
          p->q,A |- p    q,A |- r
LeftImpl  -----------------------
                 p->q,A |- r 
-}
leftImplRule :: Sequent -> [([Sequent], Rule)]
leftImplRule (Sequent assumptions [r]) =
	zip
	[
	  [Sequent assumptions [p], Sequent (q : (List.delete imp assumptions)) [r]]
	  | imp@(Implication p q) <- assumptions
	] 
	(repeat LeftImpl)

{-
            p,A |- q
RightImpl   --------
           A |- p->q
-}
rightImplRule :: Sequent -> [([Sequent], Rule)]
rightImplRule seq = case seq of 
	(Sequent assumptions [Implication p q]) -> [( [Sequent (p:assumptions) [q]], RightImpl )]
	_ -> []


--helpers
isAtomic :: Proposition -> Bool
isAtomic (Atomic _) = True
isAtomic _ = False 


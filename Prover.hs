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

leftConjRule :: Sequent -> [([Sequent], Rule)]
leftConjRule (Sequent assumptions [r]) =
	zip
	[
	  [Sequent (p:q: (List.delete pandq assumptions)) [r]]
	  | pandq@(Conjunction p q) <- assumptions
	]
	(repeat LeftConj)


{-
           A |- p    A |- q
RightConj  ----------------
              A |- p&q
-}
rightConjRule :: Sequent -> [([Sequent], Rule)]
rightConjRule seq =
	case seq of 
		(Sequent assumptions [Conjunction p q]) ->
			[( [Sequent assumptions [p], Sequent assumptions [q]], RightConj)]
		_ -> []
	

{-
          p,A |- r    q,A |- r
LeftDisj  --------------------
               p|q,A |- r
-}
leftDisjRule :: Sequent -> [([Sequent], Rule)]
leftDisjRule (Sequent assumptions [r]) =
	zip
	[
	  [Sequent (p : (List.delete porq assumptions)) [r], Sequent (q : (List.delete porq assumptions)) [r]]
	  | porq@(Disjunction p q) <- assumptions
	]
	(repeat LeftDisj)


{-
            A |- p      A |- q
RightDisj  --------    --------
           A |- p|q    A |- p|q
-}
rightDisjRule :: Sequent -> [([Sequent], Rule)]
rightDisjRule seq = 
	case seq of
		(Sequent assumptions [Disjunction p q]) ->
			zip
			[
	    	  	  [Sequent assumptions [p]], [Sequent assumptions [q]]
			]
			(repeat RightDisj)
		_ -> []


{-
          p->q,A |- p    q,A |- r
LeftImpl  -----------------------
                 p->q,A |- r 
-}
leftImplRule :: Sequent -> [([Sequent], Rule)]
leftImplRule (Sequent assumptions [r]) =
	zip
	[
	  [Sequent assumptions [p], Sequent (q : (List.delete pimpq assumptions)) [r]]
	  | pimpq@(Implication p q) <- assumptions
	] 
	(repeat LeftImpl)


{-
            p,A |- q
RightImpl   --------
           A |- p->q
-}
rightImplRule :: Sequent -> [([Sequent], Rule)]
rightImplRule seq =
	case seq of 
		(Sequent assumptions [Implication p q]) -> 
			[( [Sequent (p:assumptions) [q]], RightImpl )]
		_ -> []


--helpers
isAtomic :: Proposition -> Bool
isAtomic (Atomic _) = True
isAtomic _ = False 


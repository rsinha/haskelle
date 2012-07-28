module G4ipCalculus where

--General Approach:
--given: a goal
--step 1: check whether its an axion, if so we are done
--step 2: Use one of 6 inference rules to replace the goal by simpler subgoals. Recurse.

import List (elem, delete)
import Haskelle

data G4ipCalculus = G4ipCalculus

instance LogicalCalculus G4ipCalculus where
	rules G4ipCalculus = tacticsG4ip 

tacticsG4ip = 	(\sequent ->
		  axiomRule sequent ++
		  leftConjRule sequent ++
		  rightConjRule sequent ++
		  leftDisjRule sequent ++
		  rightDisjRule sequent ++
		  rightImplRule sequent ++
		  leftImplAtomRule sequent ++
		  leftImplConjRule sequent ++
		  leftImplDisjRule sequent ++
		  leftImplImplRule sequent
	  	)

--rules in G3ip Calculus

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


{-
LeftImpl Rule in G3 is problematic.
Applying left implication tactic to goal (a->b |- a) yields
(a->b |- a) and (b |- a) as subgoals, thus the infinite loop.

          	p->q,A |- p    q,A |- r
LeftImplG3  	-----------------------
                      p->q,A |- r 

Instead, let's replace LeftImpl by 4 special cases.
-}


{-
                p,q,A |- r
LeftImplAtom  -------------  (p atomic)
              p->q,p,A |- r
-}
leftImplAtomRule :: Sequent -> [([Sequent], Rule)]
leftImplAtomRule (Sequent assumptions [r]) =
	zip
	[ 
	  [Sequent (q : delete f assumptions) [r]]
	  | f@(Implication p@(Atomic _) q) <- assumptions, p `elem` assumptions
	] 
	(repeat LeftImplAtom)


{-
              c->(d->b),A |- e
LeftImplConj  ----------------
               c&d->b,A |- e
-}
leftImplConjRule :: Sequent -> [([Sequent], Rule)]
leftImplConjRule (Sequent assumptions [e]) =
	zip
	[ 
	  [Sequent ((Implication c (Implication d b)) : delete f assumptions) [e]]
	  | f@(Implication (Conjunction c d) b) <- assumptions
	] 
	(repeat LeftImplConj)


{-
              c->b,d->b,A |- e
LeftImplDisj  ----------------
               c|d->b,A |- e
-}
leftImplDisjRule :: Sequent -> [([Sequent], Rule)]
leftImplDisjRule (Sequent assumptions [e]) =
	zip
	[
	  [Sequent ((Implication c b) : (Implication d b) : delete f assumptions) [e]]
	  | f@(Implication (Disjunction c d) b) <- assumptions
	]
	(repeat LeftImplDisj)


{-
              d->b,c,A |- d    b,A |- e
LeftImplImpl  -------------------------
                  (c->d)->b,A |- e
-}
leftImplImplRule (Sequent assumptions [e]) =
	zip
	[
	  let as' = delete f assumptions in 
	    [Sequent (Implication d b : as') [Implication c d], Sequent (b : as') [e]]
	    | f@(Implication (Implication c d) b) <- assumptions
	] 
	(repeat LeftImplImpl)


--helpers
isAtomic :: Proposition -> Bool
isAtomic (Atomic _) = True
isAtomic _ = False 


module Prover where

import Maybe (fromJust, isJust)
import Haskelle


prove :: (Sequent -> [([Sequent], Rule)]) -> Sequent -> Maybe ProofTree 
prove tactics sequent =
	if null subtrees then Nothing else Just (ProofTree sequent rule (map fromJust $ subtree))
	where 	subtrees = [(subtree,rule) | (subgoal,rule) <- tactics sequent,
					   subtree <- [map (prove tactics) subgoal],
					   all isJust subtree]
		(subtree,rule) = head subtrees



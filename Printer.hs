module Printer where

import Haskelle

-- pretty printing

instance Show Proposition where
	show (Atomic p) = p
	show (Conjunction p1 p2) = "(" ++ show p1 ++ "&" ++ show p2 ++ ")"
	show (Disjunction p1 p2) = "(" ++ show p1 ++ "|" ++ show p2 ++ ")"
	show (Implication p1 p2) = "(" ++ show p1 ++ "->" ++ show p2 ++ ")"

instance Show Sequent where
	show (Sequent ps qs) = let unbracket xs = (init.tail) xs in
		" " ++ unbracket (show ps) ++ " |- " ++ unbracket (show qs) ++ " "

instance Show ProofTree where
	show tree = showtree 0 tree
		where showtree i (ProofTree sequent rule children) 
			= concatMap (showtree (i+4)) children
			++ replicate i ' ' ++ show sequent
			++ "    (" ++ show rule ++ ")\n"



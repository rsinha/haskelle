module Haskelle (Proposition(..), Sequent(..), Rule(..), ProofTree(..)) where

import List (delete, intersect)
import Maybe (isJust, fromJust)


--Data Structures

data Proposition = 	Atomic String |
			Conjunction Proposition Proposition |
			Disjunction Proposition Proposition |
			Implication Proposition Proposition 	deriving (Eq)

data Sequent = 		Sequent [Proposition] [Proposition]

data Rule = 		Axiom |
			LeftConj |
			RightConj |
			LeftDisj |
			RightDisj |
			LeftImpl |
			RightImpl |
			LeftImplAtom |
			LeftImplConj |
			LeftImplDisj |
			LeftImplImpl 	deriving (Show)

data ProofTree =	ProofTree Sequent Rule [ProofTree]


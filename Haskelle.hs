module Haskelle where

import List (delete, intersect)
import Maybe (isJust, fromJust)


--each Logic System must provide a set of rules where each rule transforms a Sequent into [([Sequent], Rule)] 
class LogicalCalculus system where
        rules :: system -> Sequent -> [([Sequent], Rule)]


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



--infix operators
p --> q = Implication p q 	--logical implication
ps |- qs = Sequent ps qs	--turnstile symbol from Sequent Calculus for Propositional Logic

{-
-- Curry-Howard correspondence between logic and type theory
-- a & b corresponds to product type (a,b), a | b corresponds to sum type (Either a b)
instance Num Proposition where
	p + q = Disjunction p q
	p * q = Conjunction p q
-}


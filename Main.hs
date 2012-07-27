import Haskelle
import Prover

main = do
	a <- return (Atomic "a")
	b <- return (Atomic "b")
	c <- return (Atomic "c")
	p <- return (Atomic "p")
	q <- return (Atomic "q")
	r <- return (Atomic "r")
	s <- return (Atomic "s")
	as <- return [Atomic "A"] --random propositions
	
	--sanity tests
	putStrLn "\n============\nSANITY TESTS\n============\n"
	putStrLn . show . Prover.axiomRule $ (Sequent (p:as) [p])
	putStrLn . show . Prover.leftConjRule $ (Sequent ((Conjunction p q):as) [r])
	putStrLn . show . Prover.rightConjRule $ (Sequent as [Conjunction p q])
	putStrLn . show . Prover.leftDisjRule $ (Sequent ((Disjunction p q):as) [r])
	putStrLn . show . Prover.rightDisjRule $ (Sequent as [Disjunction p q])
	putStrLn . show . Prover.leftImplRule $ (Sequent ((Implication p q):as) [r])
	putStrLn . show . Prover.rightImplRule $ (Sequent as [Implication p q])

	--random tests
	putStrLn "\n============\nRANDOM TESTS\n============\n"
	putStrLn (show (ProofTree (Sequent [] [Implication (Atomic "p") (Atomic "p")]) RightImpl [ProofTree (Sequent [Atomic "p"] [Atomic "p"]) Axiom []]))

	putStrLn . show . Prover.isAtomic $ a	
	putStrLn . show . Prover.isAtomic $ (Conjunction a b)

	putStrLn . show . Prover.leftImplRule $ (Sequent [(Implication a b)] [a])
	putStrLn . show . Prover.leftDisjRule $ (Sequent [Disjunction p q, Disjunction a b] [r])

	--prover tests
	putStrLn "\n============\nPROVER TESTS\n============\n"
	putStrLn . show . Prover.proof Prover.tactics $ (Sequent as [Implication p p])
	putStrLn . show . Prover.proof Prover.tactics $ (Sequent [] [(Implication p (Implication q p))])
	putStrLn . show . Prover.proof Prover.tactics $ (Sequent [] [(Implication (Conjunction p (Disjunction q r)) (Disjunction (Conjunction p q) (Conjunction p r)) )])
	--putStrLn . show . Prover.proof Prover.tactics $ (Sequent [] [(Implication (Implication a b) (Implication (Implication c a) (Implication c b)))])

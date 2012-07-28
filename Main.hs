import Haskelle
import G3ipCalculus
import G4ipCalculus
import Prover

main = do
	g3rules <- return (rules G3ipCalculus)
	g4rules <- return (rules G4ipCalculus)

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
	putStrLn . show . G3ipCalculus.axiomRule $ (Sequent (p:as) [p])
	putStrLn . show . G3ipCalculus.leftConjRule $ (Sequent ((Conjunction p q):as) [r])
	putStrLn . show . G3ipCalculus.rightConjRule $ (Sequent as [Conjunction p q])
	putStrLn . show . G3ipCalculus.leftDisjRule $ (Sequent ((Disjunction p q):as) [r])
	putStrLn . show . G3ipCalculus.rightDisjRule $ (Sequent as [Disjunction p q])
	putStrLn . show . G3ipCalculus.leftImplRule $ (Sequent ((Implication p q):as) [r])
	putStrLn . show . G3ipCalculus.rightImplRule $ (Sequent as [Implication p q])

	--random tests
	putStrLn "\n============\nRANDOM TESTS\n============\n"
	putStrLn (show (ProofTree (Sequent [] [Implication (Atomic "p") (Atomic "p")]) RightImpl [ProofTree (Sequent [Atomic "p"] [Atomic "p"]) Axiom []]))

	putStrLn . show . G3ipCalculus.isAtomic $ a	
	putStrLn . show . G3ipCalculus.isAtomic $ (Conjunction a b)

	putStrLn . show . G3ipCalculus.leftImplRule $ (Sequent [(Implication a b)] [a])
	putStrLn . show . G3ipCalculus.leftDisjRule $ (Sequent [Disjunction p q, Disjunction a b] [r])

	--prover tests
	putStrLn "\n============\nPROVER TESTS: G3\n============\n"
	putStrLn . show . Prover.prove g3rules $ (Sequent as [Implication p p])
	putStrLn . show . Prover.prove g3rules $ (Sequent as [Implication p q])
	putStrLn . show . Prover.prove g3rules $ (Sequent [] [(Implication p (Implication q p))])
	putStrLn . show . Prover.prove g3rules $ (Sequent [] [(Implication (Conjunction p (Disjunction q r)) (Disjunction (Conjunction p q) (Conjunction p r)) )])
	
	putStrLn "\n============\nPROVER TESTS: G4\n============\n"
	putStrLn . show . Prover.prove g4rules $ (Sequent [] [(Implication (Implication a b) (Implication (Implication c a) (Implication c b)))])

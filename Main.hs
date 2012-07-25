import Haskelle
import Prover

main = do
	putStrLn (show (Haskelle.ProofTree (Haskelle.Sequent [] [Haskelle.Implication (Haskelle.Atomic "p") (Haskelle.Atomic "p")]) Haskelle.RightImpl [Haskelle.ProofTree (Haskelle.Sequent [Haskelle.Atomic "p"] [Haskelle.Atomic "p"]) Haskelle.Axiom []]))

	putStrLn . show . Prover.isAtomic $ (Haskelle.Atomic "o")	
	putStrLn . show . Prover.isAtomic $ (Haskelle.Conjunction (Haskelle.Atomic "o1") (Haskelle.Atomic "o2"))

	a <- return (Haskelle.Atomic "a")
	b <- return (Haskelle.Atomic "b")
	putStrLn . show . Prover.leftImplRule $ (Haskelle.Sequent [(Haskelle.Implication a b)] [a])

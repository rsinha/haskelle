import Haskelle
import Printer
import Prover

main = do
	putStrLn (show (Haskelle.ProofTree (Haskelle.Sequent [] [Haskelle.Implication (Haskelle.Atomic "p") (Haskelle.Atomic "p")]) Haskelle.RightImpl [Haskelle.ProofTree (Haskelle.Sequent [Haskelle.Atomic "p"] [Haskelle.Atomic "p"]) Haskelle.Axiom []]))

	putStrLn . show . Prover.isAtomic $ (Haskelle.Atomic "o")	
	putStrLn . show . Prover.isAtomic $ (Haskelle.Conjunction (Haskelle.Atomic "o1") (Haskelle.Atomic "o2"))

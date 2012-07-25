import Haskelle
import Printer

main = do
	putStrLn (show (Haskelle.ProofTree (Haskelle.Sequent [] [Haskelle.Implication (Haskelle.Atomic "p") (Haskelle.Atomic "p")]) Haskelle.RightImpl [Haskelle.ProofTree (Haskelle.Sequent [Haskelle.Atomic "p"] [Haskelle.Atomic "p"]) Haskelle.Axiom []]))
	

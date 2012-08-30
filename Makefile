haskelle: Main.hs Haskelle.hs Prover.hs G3ipCalculus.hs G4ipCalculus.hs ParseMonad.hs ExprMonad.hs
	ghc -o haskelle Main.hs

interpret:
	runhaskell Main.hs

run:
	./haskelle

clean:
	rm -f *.o *.hi haskelle

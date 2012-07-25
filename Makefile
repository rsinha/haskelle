haskelle: Main.hs Haskelle.hs Prover.hs
	ghc -o haskelle Main.hs

interpret:
	runhaskell Main.hs

run:
	./haskelle

clean:
	rm -f *.o *.hi haskelle

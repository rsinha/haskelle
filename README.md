haskelle
========

Theorem Prover for Propositional Logic
Inspired by http://www.polyomino.f2s.com/david/haskell/programsfromproofs.html

Haskelle uses sequent calculus for proving theorems for Propositional Logic.
It is sound, i.e. all provable theorems are true propositions. 


Usage
=====

make: produces ./haskelle executable

make clean: cleans object files from compilation

To use in GHC interactive mode, :load Main.hs. This loads all dependencies.

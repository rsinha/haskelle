module Prover where

import Haskelle

isAtomic :: Proposition -> Bool
isAtomic (Atomic _) = True
isAtomic _ = False 

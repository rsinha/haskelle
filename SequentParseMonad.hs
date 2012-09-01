module SequentParseMonad where

import Haskelle
import Monad
import ParseMonad

data SeqExpr = Expr :|-: Expr 
data Expr = Con String | Expr :&: Expr | Expr :|: Expr | Expr :->: Expr deriving (Eq, Show)


evalExpr :: Expr -> Proposition
evalExpr (Con s) = Atomic s
evalExpr (e :&: f) = Conjunction (evalExpr e) (evalExpr f)
evalExpr (e :|: f) = Disjunction (evalExpr e) (evalExpr f)
evalExpr (e :->: f) = Implication (evalExpr e) (evalExpr f)

evalSeqExpr :: SeqExpr -> Sequent
evalSeqExpr ((Con " ") :|-: post) = (Sequent [] [evalExpr post])
evalSeqExpr (pre :|-: post) = (Sequent [evalExpr pre] [evalExpr post])


seqexpr :: Parser SeqExpr
seqexpr = parseNoAssumption `mplus` parseAssumption
  where
  parseAssumption = do { a <- expr; token '|'; token '-'; b <- expr; return (a :|-: b) }
  parseNoAssumption = do { token '|'; token '-'; b <- expr; return ((Con " ") :|-: b) }

expr :: Parser Expr
expr = parseCon `mplus` parseConj `mplus` parseDisj `mplus` parseImpl
  where
  parseCon = do { s <- parseStr; return (Con s) }
  parseConj = do { token '(';
		  d <- expr;
		  token '&';
		  e <- expr;
		  token ')';
		  return (d :&: e) }
  parseDisj = do { token '(';
		  d <- expr;
		  token '|';
		  e <- expr;
		  token ')';
		  return (d :|: e) }
  parseImpl = do { token '(';
		  d <- expr;
		  token '-';
		  token '>';
		  e <- expr;
		  token ')';
		  return (d :->: e) }


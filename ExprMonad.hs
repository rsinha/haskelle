module ExprMonad where

import Monad
import ParseMonad

data Expr = Con Int | Expr :+: Expr | Expr :*: Expr	deriving (Eq, Show)

eval :: Expr -> Int
eval (Con i) = i
eval (e :+: f) = eval e + eval f
eval (e :*: f) = eval e * eval f


expr :: Parser Expr
expr = parseCon `mplus` parseAdd `mplus` parseMul
  where
  parseCon = do { i <- parseInt; return (Con i) }
  parseAdd = do { token '(';
		  d <- expr;
		  token '+';
		  e <- expr;
		  token ')';
		  return (d :+: e) }
  parseMul = do { token '(';
		  d <- expr;
		  token '*';
		  e <- expr;
		  token ')';
		  return (d :*: e) }

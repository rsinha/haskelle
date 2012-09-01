--Derived from Philip Walder's Parser Monad

module ParseMonad where --(Parser, apply, parse, char, spot, token, star, plus, parseInt) where

import Char
import Monad

-- a: parsed Object, String: unparsed substring
data Parser a = Parser (String -> [(a, String)])

--Apply a parser to a string
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

-- Return parsed object
parse :: Parser a -> String -> a
parse m s = head [x | (x,t) <- apply m s, t == ""]

instance Monad Parser where
	return x = Parser (\s -> [(x,s)])
	m >>= k  = Parser (\s ->
			[ (y,u) |
			  (x,t) <- apply m s,
			  (y,u) <- apply (k x) t])

instance MonadPlus Parser where
	mzero = Parser (\s -> [])
	mplus m n = Parser (\s -> apply m s ++ apply n s)

--Parser from a predicate
spot :: (Char -> Bool) -> Parser Char
spot p = Parser f
	where
	f [] 			= []
	f (c:s) | p c 		= [(c,s)]
		| otherwise 	= []

--spot p = do { c <- char; guard (p c); return c }

--parser for a particular character
token c = spot (==c)

--parse a single character
char :: Parser Char
char = Parser f
	where
	f [] = []
	f (c:s) = [(c,s)]

--Kleene star: zero or more
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

--one or more
plus :: Parser a -> Parser [a]
plus p = do { x <- p; xs <- star p; return (x:xs) }


--Parsing a string

parseStr :: Parser String
parseStr = do { s <- plus (spot isAlpha); return s }


--Parsing an integer

--match a natural
parseNat :: Parser Int
parseNat = do { s <- plus (spot isDigit); return (read s) }

parseNeg :: Parser Int
parseNeg = do { token '-'; n <- parseNat; return (-n) }

parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg




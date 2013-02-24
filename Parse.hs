module Parse where
import Data.Char
import Data.List
import Expr
import Format

-- Line number and value
type Token = (Int, String)

-- Function from token list to list of possible parses
type Parser a = [Token] -> [(a, [Token])]

-- List of keywords
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- Parse succeeds if predicate is satisfied
pSat :: (String -> Bool) -> Parser String
pSat pred ((_, token):tokens)
    | pred token = [(token, tokens)]
    | otherwise  = []
pSat pred []     = []

-- Parse succeeds if input matches stringe exactly
pLit :: String -> Parser String
pLit s = pSat (==s)

-- Try each parser on same input and combine matches
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 tokens = (p1 tokens) ++ (p2 tokens)

-- Succeeds if input is a variable
pVar :: Parser String
pVar = pSat (\token -> isAlpha(head(token)) && token `notElem` keywords)

-- Parse input with first parser, parse remaining input with second
-- parser, and combine matches
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 tokens = do
    (match, rest)   <- p1 tokens
    (match', rest') <- p2 rest
    return (combine match match', rest')

-- Repeat parse zero or more times
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

-- Parse succeeds without consuming input
pEmpty :: a -> Parser a
pEmpty x tokens = [(x, tokens)]

-- Repeat parse one or more times
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p $ pZeroOrMore p

-- Apply function to result of parse 
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f tokens = do
    (match, rest) <- p tokens
    return (f match, rest)

-- Parse input between delimiters (discarding delimiters)
pBetween :: Parser a -> Parser b -> Parser c -> Parser b
pBetween lhs p rhs = pThen (flip const) lhs (pThen const p rhs)

-- Parse repeated input separated by delimiters (discarding delimiters)
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore (pThen (flip const) p2 p1))

-- Scan string with look-ahead 1
scan :: Int -> String -> [Token]
scan lineno [] = [(lineno, "")]
scan lineno s@(c:cs)
    | c == '\n' = scan (lineno + 1) cs
    | isSpace c = scan lineno cs
    | isDigit c = let (digits, rest) = span isDigit s
                  in (lineno, digits):scan lineno rest
    | isAlpha c = let (ending, rest) = span isIdentifierChar cs
                      identifier = c:ending
                  in (lineno, identifier):scan lineno rest
    | otherwise = scan' lineno s

-- Scanning with look-ahead 1 didn't find anything so,
-- try with look-ahead 2. Switch back to look-ahead 1 after.
scan' :: Int -> String -> [Token]
scan' lineno s@(c:c':cs)
    | cc' == "--" = let rest = dropWhile (/='\n') cs
                    in scan lineno rest
    | cc' `elem` twoCharOps = (lineno, cc'):scan lineno cs
    | otherwise = (lineno, [c]):scan lineno c'cs where
      cc' = [c, c']
      c'cs = c':cs
scan' lineno s@(c:cs) = (lineno, [c]):scan lineno cs

-- The first character of an identifier must be a letter. The
-- rest can be letters, numbers or _
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- Operators consisting of 2 characters
twoCharOps = filter ((==2) . length) $  map fst binaryOps

{-
-- Parse list of tokens into abstract syntax tree
syntax :: [Token] -> Program
syntax = undefined

parse :: [String] -> Program
parse = syntax . scan
-}


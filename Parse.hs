module Parse where
import Prelude hiding ((+))
import qualified Prelude as P
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

{- Simple lexical analyis -}

-- Scan string for integers, identifiers, and operators
scan :: Int -> String -> [Token]
scan lineno [] = []
scan lineno s@(c:cs)
    | c == '\n' = scan (lineno P.+ 1) cs                            -- Next line
    | isSpace c = scan lineno cs                                    -- Skip whitespace
    | isDigit c = let (digits, rest) = span isDigit s               -- Integer
                  in (lineno, digits):scan lineno rest
    | isAlpha c = let (ending, rest) = span isIdentifierChar cs     -- Identifier
                      identifier = c:ending
                  in (lineno, identifier):scan lineno rest
    | otherwise = scan' lineno s where                              -- Now look 2 characters ahead
        scan' lineno s@(c:c':cs)
            | cc' == "--" = let rest = dropWhile (/='\n') cs        -- Skip comments (-- Like This)
                            in scan lineno rest
            | cc' `elem` twoCharOps = (lineno, cc'):scan lineno cs  -- 2-char operators
            | otherwise = (lineno, [c]):scan lineno c'cs where      -- 1-char operators
              cc' = [c, c']
              c'cs = c':cs
        scan' lineno s@(c:cs) = (lineno, [c]):scan lineno cs        -- Not enough characters for 2-char lookahead

-- The first character of an identifier must be a letter. The
-- rest can be letters, numbers or _
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- Operators consisting of 2 characters
twoCharOps = "->":filter ((==2) . length) (map fst binaryOps)

-- This comes in handy a number of times
second = flip const

{- Parser combinators -}

-- Parse succeeds if predicate is satisfied
pSat :: (String -> Bool) -> Parser String
pSat pred ((_, token):tokens)
    | pred token = [(token, tokens)]
    | otherwise  = []
pSat pred []     = []

-- Parse succeeds if input matches stringe exactly
pLit :: String -> Parser String
pLit s = pSat (==s)

-- Succeeds if input is a variable
pVar :: Parser String
pVar = pSat (\token -> isAlpha(head(token)) && token `notElem` keywords)

-- Succeeds if input is numeric and converts to integer
pNum :: Parser Int
pNum = pApply (pSat numeric) toInt where
    toInt [] = 0
    toInt x = read x::Int
    numeric s = length s > 0 && all isDigit s

-- Minor syntactic sugar for parser alternation
(+) :: Parser a -> Parser a -> Parser a
(+) p1 p2 tokens = p1 tokens ++ p2 tokens

-- Parse input with first parser, parse remaining input with second
-- parser, and combine matches
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 tokens = do
    (match, rest)   <- p1 tokens
    (match', rest') <- p2 rest
    return (combine match match', rest')

-- Repeat parse zero or more times
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p
              + pEmpty []

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
pBetween lhs p rhs = pThen second lhs (pThen const p rhs)

-- Parse repeated input separated by delimiters (discarding delimiters)
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore (pThen second p2 p1))

{- Interface to parser -}

-- Parse String into abstract syntax tree
parse :: String -> Program
parse = syntax . scan 1

-- Parse program and then print it back out
showParse :: String -> String
showParse = showProgram . parse

-- Parse list of tokens into abstract syntax tree
syntax :: [Token] -> Program
syntax = firstParse [] . pProgram where
    firstParse _ ((program, []):_)       = program
    firstParse _ ((_, remainder):tokens) = firstParse remainder tokens
    firstParse lastFailure _             = syntaxError lastFailure

-- Try to print line number and token where last failed parse occurred
-- Fallback to ("horribly uninformative") error message =D
syntaxError remainder@(_:_) = error $ "Syntax error at line " ++ show lineno ++ ": Unexpected '" ++ token ++ "'" where
    (lineno, token)         = last remainder
syntaxError _               = error "Syntax error"

{- Productions start below -}

-- Program -> Combinator [; Combinator]*
pProgram :: Parser Program
pProgram = pOneOrMoreWithSep pCombinator (pLit ";")

-- Combinator -> var [ args]* = Expr
pCombinator :: Parser Combinator
pCombinator = pThen build pVar pArgs where
    pArgs = pThen (,) (pZeroOrMore pVar) pBody
    pBody = pThen second (pLit "=") pExpr
    build var (names, expr) = (var, names, expr)

-- Atom -> Let | LetRec | Case | Lambda | OrExpr
pExpr :: Parser Expr
pExpr = pLet
      + pOrExpr
      + pCase
      + pLambda

-- Let -> (let | letrec) Bindings LetBody
pLet = pThen build keyword rest where
    keyword = pApply (pLit "let" + pLit "letrec") (=="letrec")
    rest    = pThen (,) pBindings pLetBody
    build isRec (bindings, expr) = Let isRec bindings expr

-- Bindings -> var = Expr [; var = Expr]*
pBindings = pOneOrMoreWithSep pBinding $ pLit ";" where
    pBinding = pThen (,) pVar $ pThen second (pLit "=") pExpr

-- LetBody -> in Expr
pLetBody :: Parser Expr
pLetBody = pThen second (pLit "in") pExpr

-- Case -> case Expr of Alts
pCase :: Parser Expr
pCase = pThen build keyword rest where
    keyword = pLit "case"
    rest    = pThen (,) pExpr $ pThen second (pLit "of") pAlts
    build _ (expr, alts) = Case expr alts

-- Alts -> <tag> arg* -> Expr [; <tag> arg* -> Expr]*
pAlts :: Parser [Alt]
pAlts = pOneOrMoreWithSep pAlt $ pLit ";" where
    pAlt  = pThen build pTag pArgs
    pTag  = pBetween (pLit "<") pNum (pLit ">")
    pArgs = pThen (,) (pZeroOrMore pVar) pBody
    pBody = pThen second (pLit "->") pExpr
    build tag (args, body) = (tag, args, body)

-- Lambda -> \var+ -> Expr
pLambda :: Parser Expr
pLambda = pThen build keyword rest where
    keyword = pLit "\\"
    rest  = pThen (,) pArgs pBody
    pArgs = pOneOrMore pVar
    pBody = pThen second (pLit "->") pExpr
    build _ (args, body) = Lambda args body

-- Represent the right-hand-side of an expression 
-- May be empty
data PartialExpr = NoOp
                 | FoundOp Name Expr
                   deriving Show

-- Combine a complete expression with a partial one
assembleOp :: Expr -> PartialExpr -> Expr
assembleOp e1 NoOp            = e1
assembleOp e1 (FoundOp op e2) = App (App (Var op) e1) e2

-- OrExpr -> AndExpr [|| OrExpr]
--        -> nil
pOrExpr :: Parser Expr
pOrExpr = pThen assembleOp pAndExpr pOrPartial where
    pOrPartial = pThen FoundOp (pLit "||") pOrExpr
               + pEmpty NoOp

-- AndExpr -> RelExpr [&& AndExpr]
--         -> nil
pAndExpr :: Parser Expr
pAndExpr = pThen assembleOp pRelExpr pAndPartial where
    pAndPartial = pThen FoundOp (pLit "&&") pAndExpr
                + pEmpty NoOp

-- pRelExpr -> pAddExpr [(< | > | <= | >= | == | /=) pAddExpr]
--          -> nil
pRelExpr :: Parser Expr
pRelExpr = pThen assembleOp pAddExpr pRelPartial where
    pRelPartial = pThen FoundOp relOps pAddExpr
                + pEmpty NoOp
    relOps = pLit "<"
           + pLit ">"
           + pLit "<="
           + pLit ">="
           + pLit "=="
           + pLit "/="

-- pAddExpr -> pMulExpr [+ pAddExpr]
-- pAddExpr -> pAddExpr [- pAddExpr]
--          -> nil
pAddExpr :: Parser Expr
pAddExpr = pThen assembleOp pMulExpr pAddPartial where
pAddPartial = pThen FoundOp (pLit "+") pAddExpr
            + pThen FoundOp (pLit "-") pMulExpr
            + pEmpty NoOp

-- pMulExpr -> pAtom [* pMulExpr]
-- pMulExpr -> pAtom [/ pAtom]
--          -> nil
pMulExpr :: Parser Expr
pMulExpr = pThen assembleOp pAppExpr pMulPartial where
pMulPartial = pThen FoundOp (pLit "*") pMulExpr
            + pThen FoundOp (pLit "/") pAppExpr
            + pEmpty NoOp

-- Application -> Atom [Atom]*
pAppExpr :: Parser Expr
pAppExpr = pApply (pOneOrMore pAtom) makeSpine where
    makeSpine (x:xs) = foldl App x xs

-- Atom -> num | var | Constructor | ( Expr )
pAtom :: Parser Expr
pAtom = pApply pVar Var
      + pConstructor
      + pBetween (pLit "(") pExpr (pLit ")")
      + pApply pNum Num

-- Constructor -> Pack { tag , arity }
pConstructor :: Parser Expr
pConstructor = pThen build pPack pBracketed where
    pPack = pLit "Pack"
    pBracketed = pBetween (pLit "{") pBody (pLit "}")
    pBody = pThen (,) pTag pArity
    pTag = pNum
    pArity = pThen second (pLit ",") pNum
    build _ (tag, arity) = Cons tag arity


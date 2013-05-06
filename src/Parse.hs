module Parse (
    parseCore
) where

import Types

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Language definition for lexer
languageDef =
    emptyDef { Token.commentStart    = "{-"
             , Token.commentEnd      = "-}"
             , Token.commentLine     = "--"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
             , Token.reservedNames   = [ "let"
                                       , "letrec"
                                       , "case"
                                       , "in"
                                       , "of"
                                       , "Pack"
                                       ]
             , Token.reservedOpNames = "\\":"=":"->":(map fst precByOp)
             }

-- Generate lexer and bind names that we'll use
lexer      = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
angles     = Token.angles     lexer
natural    = Token.natural    lexer
semiSep1   = Token.semiSep1   lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer

parseCore :: String -> Program
parseCore s =
    case parse pCore "core" s of
        Left e  -> error $ show e
        Right r -> r

-- Program -> Combinator [; Combinator]*
pCore :: Parser Program
pCore = whiteSpace >> semiSep1 pCombinator

-- Combinator -> var [ args]* = Expr
pCombinator :: Parser Combinator
pCombinator = do name <- identifier
                 args <- many identifier
                 reservedOp "="
                 expr <- pExpr
                 return (name, args, expr)

-- Atom -> Let | BinOp | Case | Lambda 
pExpr :: Parser Expr
pExpr =   pLet
      <|> pBinOp
      <|> pCase
      <|> pLambda
      <?> "expression"

-- Let -> (let | letrec) Bindings in Expr
pLet :: Parser Expr
pLet = do isRec <-  (reserved "let"    >> return False)
                <|> (reserved "letrec" >> return True)
          bindings <- pBindings
          reserved "in"
          expr <- pExpr
          return $ Let isRec bindings expr

-- Bindings -> var = Expr [; var = Expr]*
pBindings :: Parser [(Name, Expr)]
pBindings = semiSep1 $
    do name <- identifier
       reservedOp "="
       expr <- pExpr
       return (name, expr)

-- Case -> case Expr of Alts
pCase :: Parser Expr
pCase = do reserved "case"
           expr <- pExpr
           reserved "of"
           alts <- pAlts
           return $ Case expr alts

-- Alts -> <tag> arg* -> Expr [; <tag> arg* -> Expr]*
pAlts :: Parser [Alt]
pAlts = do tag <- angles natural
           args <- many identifier
           reservedOp "->"
           expr <- pExpr
           rest <- try (semi >> pAlts) <|> return []
           return $ (fromInteger tag, args, expr):rest

-- Lambda -> \var+ -> Expr
pLambda :: Parser Expr
pLambda = do reservedOp "\\"
             args <- many1 identifier
             reservedOp "->"
             expr <- pExpr
             return $ Lambda args expr

-- Precedence/associativity for binary operators
pBinOp :: Parser Expr
pBinOp = buildExpressionParser binOpTable pApp where
    applyBinOp op e1 e2 = App (App (Var op) e1) e2
    parseOp op = reservedOp op >> return (applyBinOp op)
    binOpTable = [ [ Infix (parseOp "*")  AssocLeft
                   , Infix (parseOp "/")  AssocLeft
                   ]
                 , [ Infix (parseOp "+")  AssocLeft
                   , Infix (parseOp "-")  AssocLeft
                   ]
                 , [ Infix (parseOp "<")  AssocNone
                   , Infix (parseOp ">")  AssocNone
                   , Infix (parseOp "<=") AssocNone
                   , Infix (parseOp ">=") AssocNone
                   , Infix (parseOp "==") AssocNone
                   , Infix (parseOp "/=") AssocNone
                   ]
                 , [ Infix (parseOp "&&") AssocLeft
                   ]
                 , [ Infix (parseOp "||") AssocLeft
                   ]
                 ]

-- Application -> Atom [Atom]*
pApp :: Parser Expr
pApp = many1 pAtom >>= return . makeSpine where
    makeSpine (x:xs) = foldl App x xs

-- Atom -> var | num | Constructor | ( Expr )
pAtom :: Parser Expr
pAtom =   (identifier >>= return . Var)
      <|> (natural >>= return . Num . fromInteger)
      <|> pConstructor
      <|> parens pExpr
      <?> "identifier, number, constructor, or parenthesized expression"

-- Constructor -> Pack { tag , arity }
pConstructor :: Parser Expr
pConstructor = do reserved "Pack"
                  (tag, arity) <- braces pair
                  return $ Cons (fromInteger tag) (fromInteger arity)
                  where pair = do tag <- natural
                                  comma
                                  arity <- natural
                                  return (tag, arity)

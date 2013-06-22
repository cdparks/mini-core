module MiniCore.Parse (
    parseCore
) where

import MiniCore.Types

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
             , Token.reservedNames   = [ "data"
                                       , "let"
                                       , "letrec"
                                       , "case"
                                       , "in"
                                       , "of"
                                       , "Pack"
                                       , "_"
                                       ]
             , Token.reservedOpNames = "|":"\\":"=":"->":(map fst precByOp)
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
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer

parseCore :: String -> ([DataSpec], Program)
parseCore s =
    case parse pCore "core" s of
        Left e  -> error $ show e
        Right r -> r

-- Program -> DataType;* Combinator [; Combinator]*
pCore :: Parser ([DataSpec], Program)
pCore = do whiteSpace
           dataSpecs <- many pDataSpec
           program <- pCombinator `sepEndBy1` semi
           eof >> return (dataSpecs, program)

-- DataSpec-> data name = Constructor [| Constructor]*;
pDataSpec :: Parser DataSpec
pDataSpec = do reserved "data"
               name <- uppercased
               reservedOp "="
               constructors <- sepBy1 pConstructor $ reservedOp "|"
               semi >> return (name, constructors)

-- Constructor -> name [ args]*
pConstructor :: Parser Constructor
pConstructor = do name <- uppercased
                  components <- many identifier
                  return (name, components)

-- Combinator -> var [ args]* = Expr
pCombinator :: Parser Combinator
pCombinator = do name <- identifier
                 args <- many identifier
                 reservedOp "="
                 expr <- pExpr
                 return (name, args, expr)

-- Data specifications and constructors must start with an uppercase letter
uppercased :: Parser Name
uppercased = do whiteSpace
                first <- upper
                rest <- many $ alphaNum <|> char '_' <|> char '\''
                whiteSpace >> return (first:rest)

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
          bindings <- braces $ pBinding `sepEndBy1` semi
          reserved "in"
          expr <- pExpr
          return $ Let isRec bindings expr

-- Binding -> var = Expr
pBinding :: Parser (Name, Expr)
pBinding = do name <- identifier
              reservedOp "="
              expr <- pExpr
              return (name, expr)

-- Case -> case Expr of Alts
pCase :: Parser Expr
pCase = do reserved "case"
           expr <- pExpr
           reserved "of"
           alts <- braces $ pAlt `sepEndBy1` semi
           return $ ConCase expr alts

-- Alts -> <tag> arg* -> Expr [; <tag> arg* -> Expr]*
-- Alts -> Wild | Constructor -> Expr [; Constructor -> expr]*
pAlt :: Parser (Alt Name)
pAlt = do (name, components) <- pWild <|> pConstructor
          reservedOp "->"
          expr <- pExpr
          return $ (name, components, expr)

pWild :: Parser Constructor
pWild = do reserved "_"
           return $ ("_", [])

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
      -- <|> pPack
      <|> parens pExpr
      <?> "identifier, number, pack, or parenthesized expression"

{-
-- Pack -> Pack { tag , arity }
pPack :: Parser Expr
pPack = do reserved "Pack"
           (tag, arity) <- braces pair
           return $ Cons (fromInteger tag) (fromInteger arity)
           where pair = do tag <- natural
                           comma
                           arity <- natural
                           return (tag, arity)
-}


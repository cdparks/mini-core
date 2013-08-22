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
             , Token.identStart      = letter <|> char '_'
             , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
             , Token.reservedNames   = [ "data"
                                       , "let"
                                       , "letrec"
                                       , "case"
                                       , "in"
                                       , "of"
                                       , "Pack"
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
symbol     = Token.symbol     lexer
whiteSpace = Token.whiteSpace lexer

-- Data specifications and constructors must start with an uppercase letter
uppercased :: Parser Name
uppercased = do whiteSpace
                first <- upper
                rest <- many $ alphaNum <|> char '_' <|> char '\''
                whiteSpace >> return (first:rest)

-- Parser entry point
parseCore :: String -> Program
parseCore s =
    case parse pCore "core" s of
        Left e  -> error $ show e
        Right r -> r

-- Program -> Declaration*
pCore :: Parser Program
pCore = do whiteSpace
           declarations <- many pDeclaration
           eof >> return declarations

-- Declaration -> Data | Combinator
pDeclaration :: Parser Declaration
pDeclaration =  pData
            <|> pCombinator
            <?> "data type or supercombinator"

-- Data -> data name TyVar* = Constructor [| Constructor]*;
pData :: Parser Declaration
pData = do reserved "data"
           name <- uppercased
           vars <- many identifier
           reservedOp "="
           constructors <- pConstructor `sepBy1` reservedOp "|"
           semi >> return (Data name vars constructors)

-- Constructor -> name Type*
pConstructor :: Parser Constructor
pConstructor = do name <- uppercased
                  args <- many pType
                  return $ Constructor name args

-- Type -> TyCon | TyVar | (TyApp)
pType :: Parser Type
pType =  (uppercased >>= return . (flip TCon) [])
     <|> (identifier >>= return . TVar)
     <|> parens pTypeApp
     <?> "type variable or type constructor"

-- TypeApp -> TyCon Type*
pTypeApp :: Parser Type
pTypeApp = do name <- uppercased
              args <- many pType
              return $ TCon name args

-- Combinator -> var [ args]* = Expr;
pCombinator :: Parser Declaration
pCombinator = do name <- identifier
                 args <- many identifier
                 reservedOp "="
                 expr <- pExpr
                 semi >> return (Combinator name args expr)

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

-- Case -> case Expr of { Alt;+ }
pCase :: Parser Expr
pCase = do reserved "case"
           expr <- pExpr
           reserved "of"
           alts <- braces $ pAlt `sepEndBy1` semi
           return $ Case expr alts

-- Alt -> Constructor -> Expr
--pAlt = do (name, components) <- pConstructor
pAlt :: Parser Alt
pAlt = do name <- uppercased
          components <- many identifier
          reservedOp "->"
          expr <- pExpr
          return $ (PCon name, components, expr)

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
    parseOp op = reservedOp op >> return (BinOp op)
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

-- Atom -> var | num | ( Expr )
pAtom :: Parser Expr
pAtom =   (identifier >>= return . Var)
      <|> (natural >>= return . Num . fromInteger)
      <|> parens pExpr
      <?> "identifier, number, or parenthesized expression"


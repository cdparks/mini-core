{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module Format where

import Text.PrettyPrint
import Expr

-- Map binary ops to precedence
binaryOps = [("||", 2), -- Boolean OR
             ("&&", 3), -- Boolean AND
             ("==", 4), -- Comparators
             ("/=", 4),
             ("<",  4),
             (">",  4),
             ("<=", 4),
             (">=", 4),
             ("+",  6), -- Arithmetic operators
             ("-",  6),
             ("*",  7),
             ("/",  7)]

-- Function application has the highest precedence
applyPrec = 10

-- Reset precedence in unambiguous constructions
lowestPrec = 0

-- Convert expression into a formatted object
format :: Int -> Expr -> Doc
format _ (Var v) = text v
format _ (Num n) = int n
format _ (Cons tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)
format prec (App (App (Var op) e1) e2) =
    case lookup op binaryOps of
        Just prec' -> wrap expr where
            expr = format prec' e1 <+> text op <+> format prec' e2
            wrap | prec' > prec = id
                 | otherwise    = parens
        Nothing -> wrap expr where
            expr = text op <+> format applyPrec e1 <+> format applyPrec e2
            wrap | prec == applyPrec = parens
                 | otherwise         = id
format prec (App e1 e2) = wrap expr where
    expr = format applyPrec e1 <+> format applyPrec e2
    wrap | prec == applyPrec = parens
         | otherwise         = id
format _ (Let rec bindings body) =
    text keyword <+> nest indent (formatBindings lowestPrec bindings) $$ text "in" <+> format lowestPrec body
        where indent  = length keyword + 1
              keyword | rec       = "letrec"
                      | otherwise = "let"
format _ (Case scrutinee alts) =
    text "case" <+> format lowestPrec scrutinee <+> text "of" $$ nest 5 (formatAlts lowestPrec alts)
format _ (Lambda args body) =
    parens $ text "\\" <> sep (map text args) <+> text "->" <+> format lowestPrec body

-- Format name = expression pairs
formatBindings :: Int -> [(Name, Expr)] -> Doc
formatBindings prec bindings = vcat (punctuate semi (map formatBinding bindings)) where
    formatBinding (name, expr) = text name <+> text "=" <+> format prec expr

-- Format alternatives of of the form <tag> [arg ...] -> expr
formatAlts :: Int -> [Alt] -> Doc
formatAlts prec alts = vcat (punctuate semi (map formatAlt alts)) where
    formatAlt (tag, args, expr) = text "<" <> int tag <> text ">" <+> sep (map text args) <+> text "->" <+> format prec expr

{- Print things explicitly for now -}

-- Pretty-print expression
formatExpr :: Expr -> Doc
formatExpr = format lowestPrec

-- Pretty-print top-level declaration
formatCombinator :: Combinator -> Doc
formatCombinator (name, args, expr) = text name <+> sep (map text args) <+> text "=" <+> formatExpr expr

-- Pretty-print list of combinators
formatProgram :: Program -> Doc
formatProgram = sep . punctuate semi . map formatCombinator

{-
instance Show Expr where
    show = show . formatExpr

instance Show Combinator where
    show = show . formatCombinator

instance Show Program where
    show = show . formatProgram
-}


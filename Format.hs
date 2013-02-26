{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module Format where
import Data.List
import Expr

-- Represent a formatted expression as a tree of 
-- Appends and layout directives
data Format = Empty
            | String String
            | Append Format Format
            | Newline
            | Indent
            | Dedent
              deriving Show

isEmpty :: Format -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Concatenate multiple formatted values using Append
concatenate :: [Format] -> Format
concatenate = foldr Append Empty

-- Concatenate multiple formatted values with a separator
-- in between each pair
interleave :: Format -> [Format] -> Format
interleave sep = foldr join Empty where
    join left right
        | isEmpty right = left
        | otherwise     = left `Append` sep `Append` right

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
format :: Int -> Expr -> Format
format _ (Var v) = String v
format _ (Num n) = String $ show n
format _ (Cons tag arity) = concatenate [String "Pack{", String (show tag), String ", ", String (show arity), String "}"]
format prec (App (App (Var op) e1) e2) =
    case lookup op binaryOps of
        Just prec' -> if prec' > prec then
                          concatenate [format prec' e1, String " ", String op, String " ", format prec' e2]
                      else
                          concatenate [String "(", format prec' e1, String " ", String op, String " ", format prec' e2, String ")"]
        Nothing -> concatenate [String op, String " ", format applyPrec e1, String " ", format applyPrec e2]
format prec (App e1 e2)
    | prec == applyPrec = concatenate [String "(", format applyPrec e1, String " ", format applyPrec e2, String ")"]
    | otherwise         = concatenate [format applyPrec e1, String " ", format applyPrec e2]
format _ (Let rec bindings body) =
    concatenate [String keyword, Indent, Newline,
                 formatBindings lowestPrec bindings, Newline,
                 String "in ", format lowestPrec body]
    where keyword
            | recursive = "letrec"
            | otherwise = "let"
format _ (Case scrutinee alts) =
    concatenate [String "case ", format lowestPrec scrutinee, String " of", Indent, Newline,
                 formatAlts lowestPrec alts]
format _ (Lambda args body) =
    concatenate [String "(\\", String (intercalate " " args), String " -> ", format lowestPrec body, String ")"]

-- Format name = expression pairs
formatBindings :: Int -> [(Name, Expr)] -> Format
formatBindings prec bindings = interleave sep (map (formatBinding prec) bindings)
    where sep = concatenate [String ";", Newline]

-- Format a single name = expression pair
formatBinding :: Int -> (Name, Expr) -> Format
formatBinding prec (name, expr) = concatenate [String name, String " = ", format prec expr]

-- Format alternatives of of the form <tag> [arg ...] -> expr
formatAlts :: Int -> [Alt] -> Format
formatAlts prec alts = interleave sep (map (formatAlt prec) alts)
    where sep = concatenate [String ";", Newline]

-- Format a single alternative
formatAlt :: Int -> (Int, [Name], Expr) -> Format
formatAlt prec (tag, args, expr) =
    concatenate [String "<", String (show tag), String ">",
                 String (showArgs args), String " -> ", Indent, format prec expr, Dedent]

-- Convenience function to print space-separated strings
showArgs [] = ""
showArgs args@(_:_) = " " ++ intercalate " " args

-- Convert a formatted object into a string
fromFormat :: Format -> String
fromFormat fmt = flatten 0 [fmt]

-- Convert formatted object into a string keeping track of layout
flatten :: Int -> [Format] -> String
flatten depth [] = ""
flatten depth (Empty:rest) = flatten depth rest
flatten depth (String s:rest) = s ++ flatten depth rest
flatten depth (Append s1 s2:rest) = flatten depth (s1:s2:rest)
flatten depth (Indent:rest) = flatten (depth + 2) rest
flatten depth (Dedent:rest) = flatten (depth - 2) rest
flatten depth (Newline:rest) = ('\n':space depth) ++ flatten depth rest

-- Generate a string of n spaces
space :: Int -> String
space n
    | n <= 0    = ""
    | otherwise = ' ':space (n - 1)

{- Print things explicitly for now -}

-- Pretty-print expression
showExpr :: Expr -> String
showExpr = fromFormat . format 0

-- Pretty-print top-level declaration
showCombinator :: Combinator -> String
showCombinator (name, args, expr) = name ++ showArgs args ++ " = " ++ showExpr expr

-- Pretty-print list of combinators
showProgram :: Program -> String
showProgram = intercalate ";\n\n" .  map showCombinator

{-
instance Show Expr where
    show = fromFormat . format 0

instance Show Combinator where
    show (name, args, expr) = name ++ " " ++ intercalate " " args ++ " = " ++ show expr

instance Show Program where
    show = intercalate ";\n\n" .  map show
-}


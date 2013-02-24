{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

module Format where
import Data.List
import Expr

-- Represent a formatted expression as a tree of 
-- Appends and layout directives
data Format = Empty
            | String String
            | Append Format Format
            | Newline
            | Indent Format
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

-- Convert expression into a formatted object
format :: Expr -> Format
format (Var v) = String v
format (Num n) = String $ show n
format (Cons tag arity) = concatenate [String "Pack{", String (show tag), String ", ", String (show arity), String "}"]
format (App e1 e2) = concatenate [String "(", format e1, String " ", format e2, String ")"]
format (Let rec bindings body) =
    concatenate [String keyword, Newline,
                 String " ", Indent (formatBindings bindings), Newline,
                 String "in ", format body]
    where keyword
            | recursive = "letrec"
            | otherwise = "let"
format (Case scrutinee alts) =
    concatenate [String "case ", format scrutinee, String " of", Newline,
                 formatAlts alts]
format (Lambda args body) =
    concatenate [String "(\\", String (intercalate " " args), String "-> ", format body, String ")"]

-- Format name = expression pairs
formatBindings :: [(Name, Expr)] -> Format
formatBindings bindings = interleave sep (map formatBinding bindings)
    where sep = concatenate [String ";", Newline]

-- Format a single name = expression pair
formatBinding :: (Name, Expr) -> Format
formatBinding (name, expr) = concatenate [String name, String " = ", Indent (format expr)]

-- Format alternatives of of the form <tag> [arg ...] -> expr
formatAlts :: [Alt] -> Format
formatAlts alts = interleave sep (map formatAlt alts)
    where sep = concatenate [String ";", Newline]

-- Format a single alternative
formatAlt :: (Int, [Name], Expr) -> Format
formatAlt (tag, args, expr) =
    concatenate [Indent (String " <"), String (show tag), String "> ",
                 String (intercalate " " args), String "-> ", Indent (format expr)]

-- Convert a formatted object into a string
fromFormat :: Format -> String
fromFormat fmt = flatten 0 [(fmt, 0)]

-- Convert formatted object into a string keeping track of layout
flatten :: Int -> [(Format, Int)] -> String
flatten col [] = ""
flatten col ((Empty, indent):rest) = flatten col rest
flatten col ((String s, indent):rest) = s ++ flatten (col + length s) rest
flatten col ((Append s1 s2, indent):rest) = flatten col ((s1, indent):(s2, indent):rest)
flatten col ((Indent s, indent):rest) = flatten col ((s, col):rest)
flatten col ((Newline, indent):rest) = '\n':(space indent) ++ flatten indent rest

-- Generate a string of n spaces
space :: Int -> String
space n
    | n <= 0    = ""
    | otherwise = ' ':space (n - 1)

instance Show Expr where
    show = fromFormat . format

instance Show Combinator where
    show (name, args, expr) = name ++ " " ++ intercalate " " args ++ " = " ++ show expr

instance Show Program where
    show = intercalate "\n\n" .  map show


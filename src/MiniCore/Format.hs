{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module MiniCore.Format (
    format,
    formatStateOutput,
    formatState,
    formatDefs,
    precByOp,
    Doc (..)
) where

import MiniCore.Types
import MiniCore.Heap

import Text.PrettyPrint
import Data.List

-- Function application has the highest precedence
applyPrec = 10

-- Reset precedence in unambiguous constructions
lowestPrec = 0

-- Join a list of strings together
join :: [String] -> Doc
join = sep . map text

{- Pretty-print program source -}

-- Format typeclass defines how a value should be pretty-printed
class Format a where
    format :: a -> Doc
    format = formatPrec lowestPrec

    formatPrec :: Int -> a -> Doc
    formatPrec = const format

-- Pretty-print list of declarations
instance Format Program where
    format = sep . punctuate semi . map format

-- Pretty-print a declaration (data or combinator)
instance Format Declaration where
    format (Combinator name args expr) =
        text name <+> join args <+> text "=" <+> format expr
    format (Data name vars constructors) =
        text "data" <+> text name <+>
        join vars <+> text "=" <+>
        sep (punctuate (text " |") $ map format constructors)

-- Pretty-print a constructor
instance Format Constructor where
    format (Constructor name components) =
        text name <+> sep (map format components)

{- Pretty-printing types -}

-- Infinite list of prettier type-variables
prettyVars :: [String]
prettyVars = alphas ++ alphaNums
  where alphas =
            do char <- ['a'..'z']
               return [char]
        alphaNums =
            do num  <- [1..]
               char <- ['a'..'z']
               return $ char:show num

-- Find name in env (or just use name if not found) and convert to text
lookupText :: [(Name, Name)] -> Name -> Doc
lookupText env n = maybe (text n) text (lookup n env)

-- Print a type using a mapping from the actual
-- type-variables to pretty type-variables
prettyType :: [(Name, Name)] -> Type -> Doc
prettyType env t = loop False False t
  where
    loop _ _ (TVar n) = lookupText env n
    loop _ _ (TCon n []) = text n

    -- If first parameter is True, parent was (->), and current node
    -- is the left-hand side of (->) and may need to be parenthesized
    loop parent _ (TCon "(->)" [a, b]) =
        parensIf parent (loop True False a <+> text "->" <+> loop False False b)

    -- If second parameter is True, parent was a non-unary constructor
    -- and current node may need to be parameterized
    loop _ parent (TCon n xs) =
        parensIf parent (text n <+> sep (map (loop False True) xs))

-- Print universally quantified type variables if there are any
prettyForall :: [(Name, Name)] -> [Name] -> Doc
prettyForall env [] = empty
prettyForall env xs = text "forall" <+> sep (map (lookupText env) xs) <> text "."

-- Pretty-print a concrete type
instance Format Type where
    format = prettyType []

-- Pretty-print a universally quantified type
instance Format Scheme where
    format (Scheme vars t) =
        let env = zip (nub (vars ++ tvarsOrdered t)) prettyVars
        in prettyForall env vars <+> prettyType env t

-- Pretty-print a mapping of Names to Schemes
instance Format [(Name, Scheme)] where
    format = vcat . map combine
      where
        combine (name, scheme) = text name <+> text "::" <+> format scheme

-- Parenthesize if some condition is true
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- Useful down below
isApp :: Expr -> Bool
isApp (App _ _) = True
isApp _         = False

-- Convert expression into a formatted object keeping track
-- of precedence
instance Format Expr where
    -- Variable
    formatPrec _ (Var v) = text v

    -- Number
    formatPrec _ (Num n) = int n

    -- Pack operator
    formatPrec _ (Cons tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)

    -- Binary application
    formatPrec prec (BinOp op e1 e2) =
        case lookup op precByOp of
            Just prec' -> parensIf (prec' < prec) $
                formatPrec prec' e1 <+> text op <+> formatPrec prec' e2
            Nothing -> error $ "Unrecognized infix operator " ++ op

    -- Prefix application
    formatPrec prec (App e1 e2) =
        formatPrec applyPrec e1 <+> parensIf (isApp e2) (formatPrec applyPrec e2)

    -- Let expression
    formatPrec _ (Let recursive bindings body) =
        text keyword <+> lbrace $$ nest 2 (format bindings) $$
        rbrace <+> text "in" <+> format body
            where keyword | recursive = "letrec"
                          | otherwise = "let"

    -- Case expression
    formatPrec _ (Case scrutinee alts) =
        text "case" <+> format scrutinee <+>
        text "of" <+> lbrace $$ nest 2 (format alts) $$ rbrace

    -- Lambda expression
    formatPrec prec (Lambda args body) =
        parensIf (prec > lowestPrec) $
            text "\\" <> join args <+>
            text "->" <+> formatPrec lowestPrec body

-- Format name = expression pairs
instance Format [(Name, Expr)] where
    formatPrec prec bindings =
        vcat (punctuate semi (map formatBinding bindings)) <> semi where
            formatBinding (name, expr) = text name <+> text "=" <+> formatPrec prec expr

-- Format alternatives of of the form Pattern [arg ...] -> expr
instance Format [Alt] where
    formatPrec prec alts =
        vcat (punctuate semi (map formatAlt alts)) <> semi where
            formatAlt (pattern, args, expr) = format pattern <+> join args <+>
                text "->" <+> formatPrec prec expr

-- Format constructor, wildcard, or internal tagged pattern
instance Format Pattern where
    format (PCon constructor) = text constructor
    format (PTag tag)         = text "<" <> int tag <> text ">"

{- Pretty-print machine states -}

-- Format output component of state
formatStateOutput :: GMState -> Doc
formatStateOutput = text . concat . reverse . gmOutput

-- Format global definitions
formatDefs :: GMState -> Doc
formatDefs state = text "Definitions" <> colon $$ nest 4 defs where
    defs = vcat $ map (formatSC state) $ gmGlobals state

-- Format a single supercombinator
formatSC :: GMState -> (Name, Addr) -> Doc
formatSC state (name, addr) = text name <> colon $$ nest 4 (formatCode code) where
    (NGlobal _ code) = hLoad (gmHeap state) addr

-- Format stack and current code
formatState :: GMState -> Doc
formatState state =
    text "State" <+> parens (formatStats $ gmStats state) <> colon $$
    nest 4 (formatStack state $$
            formatVStack state $$
            formatCode (gmCode state) $$
            formatDump state)

-- Format number of steps and collections
formatStats :: GMStats -> Doc
formatStats stats =
    text "step" <+> int (gmSteps stats) <> comma <+>
    text "gc" <+> int (gmCollections stats)

-- Format nodes on stack
formatStack :: GMState -> Doc
formatStack state =
    text "Stack" <> colon $$
    nest 4 (vcat $ map (formatNode state) (reverse (gmStack state)))

-- Format numbers in V-stack
formatVStack :: GMState -> Doc
formatVStack state =
    text "V-Stack" <> colon $$
    nest 4 (vcat $ map (text . show) (reverse (gmVStack state)))

-- Format first n addresses on stack 
formatShortStack :: GMStack -> Int -> Doc
formatShortStack stack n =
    text "Stack" <> colon <+>
    hsep (punctuate comma $ shorten n $ map formatAddr $ reverse stack)

-- Format list of instructions
formatCode :: GMCode -> Doc
formatCode code = text "Code" <> colon $$ nest 4 (vcat $ map (text . show) code)

-- Format first n instructions
formatShortCode :: GMCode -> Int -> Doc
formatShortCode code n =
    text "Code" <> colon <+>
    hsep (punctuate comma $ shorten n $ map (text . show) code)

-- Only use first n docs in list. Append ellipsis if docs longer than n.
shorten :: Int -> [Doc] -> [Doc]
shorten n docs
    | length docs > n = take n docs ++ [text "..."]
    | otherwise       = docs

-- Format dump
formatDump :: GMState -> Doc
formatDump state = format $ gmDump state where
    format []                = empty
    format (([], _, _):_)       = empty
    format ((_, [], _):_)       = empty
    format ((code, stack, vstack):_) =
        text "Dump" <> colon $$
        nest 4 (formatShortStack stack 3 $$ formatShortCode code 3)

-- Format a single node
formatNode :: GMState -> Addr -> Doc
formatNode state addr = formatAddr addr <> colon <+> draw (hLoad (gmHeap state) addr) where
    draw (NNum n) = int n
    draw (NGlobal n g) = text "Global" <+> text v where
        (v, _) = head (filter (\(x, b) -> b == addr) (gmGlobals state))
    draw (NApp a1 a2) = text "App" <+> formatAddr a1 <+> formatAddr a2
    draw (NPointer a) = text "Pointer to" <+> formatAddr a
    draw (NConstructor tag components) =
        text "Cons" <+> int tag <+> brackets (hsep $ map formatAddr components)

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


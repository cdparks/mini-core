{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module Format (
    formatExpr,
    formatCombinator,
    formatProgram,
    formatLast,
    formatState,
    formatResults,
    precByOp
) where

import Types
import Heap

import Text.PrettyPrint
import Data.List

-- Function application has the highest precedence
applyPrec = 10

-- Reset precedence in unambiguous constructions
lowestPrec = 0

{- Pretty-print program source -}

-- Pretty-print list of combinators
formatProgram :: Program -> Doc
formatProgram = sep . punctuate semi . map formatCombinator

-- Pretty-print top-level declaration
formatCombinator :: Combinator -> Doc
formatCombinator (name, args, expr) = text name <+> sep (map text args) <+> text "=" <+> formatExpr expr

-- Convert expression into a formatted object
formatExpr :: Expr -> Doc
formatExpr = formatPrec lowestPrec where

-- Convert expression into a formatted object keeping track
-- of precedence
formatPrec :: Int -> Expr -> Doc
formatPrec _ (Var v) = text v
formatPrec _ (Num n) = int n
formatPrec _ (Cons tag arity) = text "Pack" <> braces (int tag <> comma <> int arity)
formatPrec prec (App (App (Var op) e1) e2) =
    case lookup op precByOp of
        Just prec' -> wrap expr where
            expr = formatPrec prec' e1 <+> text op <+> formatPrec prec' e2
            wrap | prec' > prec = id
                 | otherwise    = parens
        Nothing -> wrap expr where
            expr = text op <+> formatPrec applyPrec e1 <+> formatPrec applyPrec e2
            wrap | prec == applyPrec = parens
                 | otherwise         = id
formatPrec prec (App e1 e2) = wrap expr where
    expr = formatPrec applyPrec e1 <+> formatPrec applyPrec e2
    wrap | prec == applyPrec = parens
         | otherwise         = id
formatPrec _ (Let recursive bindings body) =
    text keyword <+> nest indent (formatBindings lowestPrec bindings) $$ text "in" <+> format lowestPrec body
        where indent  = length keyword + 1
              keyword | recursive = "letrec"
                      | otherwise = "let"
format _ (Case scrutinee alts) =
    text "case" <+> formatPrec lowestPrec scrutinee <+> text "of" $$ nest 5 (formatAlts lowestPrec alts)
format _ (Lambda args body) =
    parens $ text "\\" <> sep (map text args) <+> text "->" <+> formatPrec lowestPrec body

-- Format name = expression pairs
formatBindings :: Int -> [(Name, Expr)] -> Doc
formatBindings prec bindings = vcat (punctuate semi (map formatBinding bindings)) where
    formatBinding (name, expr) = text name <+> text "=" <+> formatPrec prec expr

-- Format alternatives of of the form <tag> [arg ...] -> expr
formatAlts :: Int -> [Alt] -> Doc
formatAlts prec alts = vcat (punctuate semi (map formatAlt alts)) where
    formatAlt (tag, args, expr) = text "<" <> int tag <> text ">" <+> sep (map text args) <+> text "->" <+> formatPrec prec expr

{- Pretty-print machine states -}

-- Print output from last state
formatLast :: [GMState] -> Doc
formatLast = text . show . (intercalate " ") . reverse . gmOutput . last

-- Format output
formatResults :: [GMState] -> Doc
formatResults states = text "Definitions" <> colon $$ nest 4 defs $$ text "Transitions" <> colon $$ nest 4 trans where
    defs = vcat $ map (formatSC state) $ gmGlobals state
    state:_ = states
    trans = vcat $ map formatState (zip states [0..])

-- Format a single supercombinator
formatSC :: GMState -> (Name, Addr) -> Doc
formatSC state (name, addr) = text name <> colon $$ nest 4 (formatCode code) where
    (NGlobal _ code) = hLoad (gmHeap state) addr

-- Format stack and current code
formatState :: (GMState, Int) -> Doc
formatState (state, n) = text "State" <+> int n <> colon $$ nest 4 (formatStack state $$ formatVStack state $$ formatCode (gmCode state) $$ formatDump state)

-- Format nodes on stack
formatStack :: GMState -> Doc
formatStack state = text "Stack" <> colon $$ nest 4 (vcat $ map (formatNode state) (reverse (gmStack state)))

-- Format numbers in V-stack
formatVStack :: GMState -> Doc
formatVStack state = text "V-Stack" <> colon $$ nest 4 (vcat $ map (text . show) (reverse (gmVStack state)))

-- Format first n addresses on stack 
formatShortStack :: GMStack -> Int -> Doc
formatShortStack stack n = text "Stack" <> colon <+> hsep (punctuate comma $ shorten n $ map formatAddr $ reverse stack)

-- Format list of instructions
formatCode :: GMCode -> Doc
formatCode code = text "Code" <> colon $$ nest 4 (vcat $ map (text . show) code)

-- Format first n instructions
formatShortCode :: GMCode -> Int -> Doc
formatShortCode code n = text "Code" <> colon <+> hsep (punctuate comma $ shorten n $ map (text . show) code)

-- Only use first n docs in list. Append ellipsis if docs longer than n.
shorten :: Int -> [Doc] -> [Doc]
shorten n docs
    | length docs > n = take n docs ++ [text "..."]
    | otherwise       = docs

-- Format dump
formatDump :: GMState -> Doc
formatDump state = format $ gmDump state where
    format []                = empty
    format (([], _):_)       = empty
    format ((_, []):_)       = empty
    format ((code, stack):_) = text "Dump" <> colon $$ nest 4 (formatShortStack stack 3 $$ formatShortCode code 3)

-- Format a single node
formatNode :: GMState -> Addr -> Doc
formatNode state addr = formatAddr addr <> colon <+> draw (hLoad (gmHeap state) addr) where
    draw (NNum n) = int n
    draw (NGlobal n g) = text "Global" <+> text v where
        (v, _) = head (filter (\(x, b) -> b == addr) (gmGlobals state))
    draw (NApp a1 a2) = text "App" <+> formatAddr a1 <+> formatAddr a2
    draw (NPointer a) = text "Pointer to" <+> formatAddr a
    draw (NConstructor tag components) =  text "Cons" <+> int tag <+> brackets (hsep $ map formatAddr components)

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


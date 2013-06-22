{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}

module MiniCore.Format (
    formatExpr,
    formatDeclaration,
    formatProgram,
    formatLast,
    formatState,
    formatResults,
    formatFirstStep,
    formatLastStep,
    formatStep,
    precByOp
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

-- Pretty-print list of declarations
formatProgram :: Program -> Doc
formatProgram = sep . map formatDeclaration

-- Pretty-print a declaration (data or combinator)
formatDeclaration :: Declaration -> Doc
formatDeclaration (Combinator name args expr) =
    text name <+> join args <+> text "=" <+> formatExpr expr <> semi
formatDeclaration (DataSpec name constructors) =
    text "data" <+> text name <+> text "=" <+>
    sep (intersperse (char '|') $ map formatConstructor constructors) <> semi

-- Pretty-print a constructor
formatConstructor :: Constructor -> Doc
formatConstructor (name, components) = text name <+> join components

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
    text keyword <+> lbrace $$ nest 2 (formatBindings lowestPrec bindings) $$
    rbrace <+> text "in" <+> formatPrec lowestPrec body
        where keyword | recursive = "letrec"
                      | otherwise = "let"
formatPrec _ (TagCase scrutinee alts) =
    text "case" <+> formatPrec lowestPrec scrutinee <+>
    text "of" <+> lbrace $$ nest 2 (formatTagAlts lowestPrec alts) $$ rbrace
formatPrec _ (ConCase scrutinee alts) =
    text "case" <+> formatPrec lowestPrec scrutinee <+>
    text "of" <+> lbrace $$ nest 2 (formatConAlts lowestPrec alts) $$ rbrace
formatPrec _ (Lambda args body) =
    parens $ text "\\" <> join args <+>
    text "->" <+> formatPrec lowestPrec body

-- Format name = expression pairs
formatBindings :: Int -> [(Name, Expr)] -> Doc
formatBindings prec bindings = vcat (punctuate semi (map formatBinding bindings)) <> semi where
    formatBinding (name, expr) = text name <+> text "=" <+> formatPrec prec expr

-- Format alternatives of of the form <tag> [arg ...] -> expr
formatTagAlts :: Int -> [Alt Int] -> Doc
formatTagAlts prec alts = vcat (punctuate semi (map formatAlt alts)) <> semi where
    formatAlt (tag, args, expr) = text "<" <> int tag <> text ">" <+>
        join args <+> text "->" <+> formatPrec prec expr

-- Format alternatives of of the form Constructor [arg ...] -> expr
formatConAlts :: Int -> [Alt Name] -> Doc
formatConAlts prec alts = vcat (punctuate semi (map formatAlt alts)) <> semi where
    formatAlt (name, args, expr) = text name <+> join args <+>
        text "->" <+> formatPrec prec expr

{- Pretty-print machine states -}

-- Print output from last state
formatLast :: [GMState] -> Doc
formatLast = text . intercalate " " . reverse . gmOutput . last

-- Format global definitions, all states, and final output
formatResults :: [GMState] -> Doc
formatResults states =
    formatDefs states $$
    formatTransitions states $$
    formatOutput states

-- Format first step for incremental output
formatFirstStep :: GMState -> Doc
formatFirstStep state = formatDefs [state]

-- Format intermediate step
formatStep :: GMState -> Doc
formatStep = formatState

-- Format last step for incremental output
formatLastStep :: GMState -> Doc
formatLastStep state = formatLast [state]

-- Format global definitions
formatDefs :: [GMState] -> Doc
formatDefs (state:_) = text "Definitions" <> colon $$ nest 4 defs where
    defs = vcat $ map (formatSC state) $ gmGlobals state

-- Format each transition
formatTransitions :: [GMState] -> Doc
formatTransitions states = text "Transitions" <> colon $$ nest 4 trans where
    trans = vcat $ map formatState states

-- Format final output
formatOutput :: [GMState] -> Doc
formatOutput states = text "Output" <> colon <+> formatLast states

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


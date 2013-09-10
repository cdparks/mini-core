{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MiniCore.Types where

import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

-- Represent a stage in the compiler as something that
-- fails with an error message or produces a value
type Stage = ErrorT String IO

-- Run compiler stage
runStage :: Stage a -> IO (Either String a)
runStage = runErrorT

-- Run compiler stage and print result
runStageIO :: Show a => Stage a -> IO ()
runStageIO s = 
    do result <- runStage s
       case result of
           Left error   -> putStrLn error
           Right result -> putStrLn $ show result

-- Print something during execution
trace :: (Show a) => a -> Stage ()
trace = traceStr . show

-- Print String during execution
traceStr :: String -> Stage ()
traceStr = liftIO . putStrLn

-- Print something for stage when prompted
traceStage :: (Show a) => String -> Bool -> a -> Stage ()
traceStage stage cond x =
    do let divider = "===================="
       when cond $
           do traceStr $ divider ++ " " ++ stage ++ " " ++ divider
              trace x
              traceStr $ "\n"

{- Core Expression types -}

-- A program is a list of declarations
type Program = [Declaration]

-- A declaration is a super-combinator or a data-type declaration
data Declaration = Combinator Name [Name] Expr
                 | Data Name [Name] [Constructor]
                   deriving Show

-- Is declaration a Data declaration or a Combinator?
isData :: Declaration -> Bool
isData (Data {}) = True
isData _         = False

-- A constructor has a name and a list of components
data Constructor = Constructor Name [Type]
                   deriving Show

-- An expression is a variable, number, Cons (Pack) operation, application,
-- let, case, or lambda expression.
data Expr = Var Name
          | Num Int
          | Cons Int Int
          | BinOp Name Expr Expr
          | App Expr Expr
          | Let IsRec [(Name, Expr)] Expr
          | Case Expr [Alt]
          | Lambda [Name] Expr
            deriving (Show)

type Name       = String
type IsRec      = Bool

-- Case alternative contains some value we can match on,
-- a list of names to bind, and a body
type Alt = (Pattern, [Name], Expr)

-- Pattern is a constructor name or an integer tag (internal representation)
data Pattern = PCon Name
             | PTag Int
               deriving Show

-- Get binders and bindees
bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

bindeesOf :: [(a, b)] -> [b]
bindeesOf = map snd

-- Map binary ops to precedence
precByOp :: [(Name, Int)]
precByOp = [ ("||", 2) -- Boolean OR
           , ("&&", 3) -- Boolean AND
           , ("==", 4) -- Comparators
           , ("/=", 4)
           , ("<",  4)
           , (">",  4)
           , ("<=", 4)
           , (">=", 4)
           , ("+",  6) -- Arithmetic operators
           , ("-",  6)
           , ("*",  7)
           , ("/",  7)
           ]

{- Annotated Expression types -}

--An AExpr is an expression annotated with some extra useful
--information. a is the type of binders in expressions, and b
--is the type of annotations.
data AExpr a b = AVar Name
               | ANum Int
               | ACons Int Int
               | AApp (Annotated a b) (Annotated a b)
               | ALet IsRec [ADef a b] (Annotated a b)
               | ACase (Annotated a b) [AAlt a b]
               | ALambda [a] (Annotated a b)
                 deriving (Show)

type Annotated a b = (b, AExpr a b)
type ADef a b      = (a, Annotated a b)
type AAlt a b      = (Pattern, [a], (Annotated a b))
type AProgram a b  = [(Name, [a], Annotated a b)]

{- Types for type checking -}

-- Concrete type
data Type = TVar Name
          | TCon Name [Type]
            deriving (Show, Eq)

-- Universally quantified types
data Scheme = Scheme [Name] Type
              deriving (Show, Eq)

-- Constructors for built-in types
intTy = TCon "Int" []
boolTy = TCon "Bool" []

-- Constructor for function types
arrow :: Type -> Type -> Type
arrow a b = TCon "(->)" [a, b]

-- Constructor for type variables
var = TVar

-- Substitute type for name
type Subst = Map.Map Name Type

-- Mapping from names to schemes
type TypeEnv = Map.Map Name Scheme

-- If something is type-like, we can apply a substitution to it
-- and get a set of its type-variables
class Types a where
    apply :: Subst -> a -> a
    tvars :: a -> Set.Set Name

instance Types Type where
    apply s (TVar n) = case Map.lookup n s of
                         Just t  -> t
                         Nothing -> TVar n
    apply s (TCon n ts) = TCon n $ map (apply s) ts

    tvars (TVar n) = Set.singleton n
    tvars (TCon n ts) = foldr Set.union Set.empty (map tvars ts)

-- Special case; pretty-printed types should have the type-variables in order
-- which tvars won't necessarily maintain
tvarsOrdered :: Type -> [Name]
tvarsOrdered (TVar n) = [n]
tvarsOrdered (TCon n ts) = foldr List.union [] (map tvarsOrdered ts)

-- tvars doesn't return universally quantified type-variables
instance Types Scheme where
    apply s (Scheme vs t) =
        let s' = foldr Map.delete s vs
        in Scheme vs (apply s' t)

    tvars (Scheme vs t) = tvars t `Set.difference` Set.fromList vs

-- Extend operations to lists of things that are type-like
instance Types a => Types [a] where
    apply s = map (apply s)
    tvars   = foldr Set.union Set.empty . map tvars

instance Types TypeEnv where
    apply s env = Map.map (apply s) env
    tvars env = tvars (Map.elems env)

-- Compose substitions by applying s1 to s2 and then
-- taking their union
scomp :: Subst -> Subst -> Subst
scomp s1 s2 = let s2' = Map.map (apply s1) s2
              in s2' `Map.union` s1

{- Heap types -}

type Addr = Int

-- (size, max-size, free-list, environment mapping addresses to live objects)
data Heap a = Heap
    { hSize        :: Int
    , hMaxSize     :: Int
    , hFreeList    :: [Addr]
    , hEnvironment :: [(Addr, a)]
    }

{- G-Compiler/Machine types -}

-- Output is just a list of Strings
type GMOutput = [String]

-- Code is just a list of instructions
type GMCode = [Instruction]

data Instruction = Pushglobal Name          -- Push address of global on stack
                 | Pushint Int              -- Push address of integer on stack
                 | Pushbasic Int            -- Push unboxed integer on V-stack
                 | Push Int                 -- Push address of local variable on stack
                 | Pop Int                  -- Pop n items from stack
                 | Slide Int                -- Pop n items from stack leaving top-of-stack
                 | Alloc Int                -- Allocate n pointers and put addresses on stack
                 | Mkap                     -- Make application node out of top two address
                 | Mkint                    -- Box integer on top of V-stack into heap and onto stack
                 | Mkbool                   -- Box Boolean on top of V-stack into heap and onto stack
                 | Get                      -- Put top-of-stack on V-stack
                 | Update Int               -- Replace root of redex with pointer to value
                 | Eval                     -- Evaluate top-of-stack to Weak Head Normal Form
                 | Unwind                   -- Unwind application nodes onto stack
                 | Add                      -- Arithmetic instructions
                 | Sub
                 | Mul
                 | Div
                 | Neg
                 | Eq                       -- Relational instructions
                 | Ne
                 | Lt
                 | Le
                 | Gt
                 | Ge
                 | Pack Int Int             -- Build NConstructor node
                 | Casejump [(Int, GMCode)] -- Use tag of node on top-of-stack to jump to case-alternative
                 | Split Int                -- Destructure constructor into components for alternative-body
                 | Cond GMCode GMCode       -- Simplified case-jump. Check top-of-V-stack to branch
                 | Print                    -- Add value to output
                 | LParen                   -- Write open paren to output
                 | RParen                   -- Write close paren to output
                 | Space                    -- Write space to output
                   deriving Show

-- Execution stack
type GMStack = [Addr]

-- Value stack for arithmetic
type GMVStack = [Int]

-- Save machine's current context during
-- strict evaluation
type GMDump = [(GMCode, GMStack, GMVStack)]

-- Heap of live objects
type GMHeap = Heap Node

-- Heap data
data Node = NNum Int
          | NApp Addr Addr
          | NGlobal Int GMCode
          | NPointer Addr
          | NConstructor Int [Addr]
          | NMarked Node
            deriving Show

-- Global environment maps names to addresses
type GMGlobals = [(Name, Addr)]

-- Current local environment maps names to stack offsets
type GMEnvironment = [(Name, Int)]

-- Tally information about machine state
data GMStats = GMStats
    { gmSteps :: Int
    , gmCollections :: Int
    }

-- Names of constructors in tag order
type GMCons = [Name]

-- Complete machine state
data GMState = GMState
    { gmOutput  :: GMOutput
    , gmCode    :: GMCode
    , gmStack   :: GMStack
    , gmDump    :: GMDump
    , gmVStack  :: GMVStack
    , gmHeap    :: GMHeap
    , gmGlobals :: GMGlobals
    , gmStats   :: GMStats
    , gmCons    :: GMCons
    }

-- Simple instance for now
instance Show GMState where
    show _ = "GMState#"


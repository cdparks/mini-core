module Types where

{- Core Expression types -}

data Expr = Var Name
          | Num Int
          | Cons Int Int
          | App Expr Expr
          | Let IsRec [(Name, Expr)] Expr
          | Case Expr [Alt]
          | Lambda [Name] Expr
            deriving (Show)

type Name       = String
type IsRec      = Bool
type Alt        = (Int, [Name], Expr)
type Combinator = (Name, [Name], Expr)
type Program    = [Combinator]

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

{- Heap types -}

type Addr = Int

-- (size, free-list, environment mapping addresses to live objects)
type Heap a = (Int, [Addr], [(Addr, a)])

{- G-Compiler/Machine types -}

-- Output is just a list of Strings
type GMOutput = [String]

-- Code is just a list of instructions
type GMCode = [Instruction]

data Instruction = Pushglobal Name          -- Push address of global on stack
                 | Pushcons Name Int Int    -- Push address of wrapped constructor on stack
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
            deriving Show

-- Global environment maps names to addresses
type GMGlobals = [(Name, Addr)]

-- Current local environment maps names to stack offsets
type GMEnvironment = [(Name, Int)]

-- Tally information about machine state
data GMStats = GMStats
    { gmSteps :: Int }

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
    }

-- Simple instance for now
instance Show GMState where
    show _ = "GMState#"


module GMachine where

import Heap
import Expr
import Parse (parseCore)

import Control.Arrow (second)
import Data.List
import Text.PrettyPrint
import Debug.Trace

-- Code is just a list of instructions
type GMCode = [Instruction]
data Instruction = Pushglobal Name      -- Push address of global on stack
                 | Pushint Int          -- Push address of integer on stack
                 | Push Int             -- Push address of local variable on stack
                 | Pop Int              -- Pop n items from stack
                 | Slide Int            -- Pop n items from stack leaving top-of-stack
                 | Alloc Int            -- Allocate n pointers and put addresses on stack
                 | Mkap                 -- Make application node out of top two address
                 | Update Int           -- Replace root of redex with pointer to value
                 | Unwind               -- Unwind application nodes onto stack
                   deriving Show

type GMStack = [Addr]
type GMHeap = Heap Node

-- Heap data
data Node = NNum Int
          | NApp Addr Addr
          | NGlobal Int GMCode
          | NPointer Addr
            deriving Show

-- Global environment maps names to addresses
type GMGlobals = [(Name, Addr)]

-- Current local environment maps names to stack offsets
type GMEnvironment = [(Name, Int)]

-- Tally information about machine state
data GMStats = GMStats {
    gmSteps :: Int
}

-- Complete machine state
data GMState = GMState {
    gmCode    :: GMCode,
    gmStack   :: GMStack,
    gmHeap    :: GMHeap,
    gmGlobals :: GMGlobals,
    gmStats   :: GMStats
}

-- Simple instance for now
instance Show GMState where
    show _ = "GMState#"

-- Extra definititions to add to the initial global environment
extraDefs = []

-- Compile and run program
run :: String -> String
run = show . formatResults . eval . compile . parseCore

-- Parse and compile program and evaluate first state.
-- Used for interactive debugging in ghci.
-- example:
-- ghci> start "main = I 10"
--   { lots of output }
-- ghci> next it
--   { lots of output }
-- ghci> next it
--   ...
start :: String -> IO GMState
start p = do
    state <- return (single (compile (parseCore p)))
    putStrLn $ show $ formatResults [state]
    return state

-- Transition to next state
-- Used for interactive debugging in ghci.
next :: GMState -> IO GMState
next state = do
    state' <- return (single state)
    putStrLn $ show $ formatState (state', 0)
    return state'

-- Turn program into initial G-Machine state
compile :: Program -> GMState
compile program = GMState codeInit [] heap globals statInit where
    (heap, globals) = buildInitialHeap program

-- Push main and unwind
codeInit :: GMCode
codeInit = [Pushglobal "main", Unwind]

-- Start at step zero
statInit :: GMStats
statInit = GMStats 0

-- Instantiate supercombinators in heap
buildInitialHeap :: Program -> (GMHeap, GMGlobals)
buildInitialHeap program = mapAccumL allocSC hInit compiled where
    compiled = map compileSC $ prelude ++ extraDefs ++ program

-- Allocate a supercombinator and return the new heap
allocSC :: GMHeap -> (Name, Int, GMCode) -> (GMHeap, (Name, Addr))
allocSC heap (name, arity, instructions) = (heap', (name, addr)) where
    (heap', addr) = hAlloc heap (NGlobal arity instructions)

-- Compile supercombinator f with formal parameters x1...xn by
-- compiling f's body e in the environment created by substituting
-- the actual parameters for the formal parameters
-- SC(f x1 ... xn = e) = R(e) [x1 -> 0, ..., xn -> n - 1] n
compileSC :: (Name, [Name], Expr) -> (Name, Int, GMCode)
compileSC (name, env, body) = (name, length env, compileR (zip env [0..]) body)

-- Compile the expression e in the environment p for a
-- supercombinator of arity d by generating code which
-- instantiates e and then unwinds the resulting stack
-- R(e) p d = C(e) p ++ [Update d, Pop d, Unwind]
compileR :: GMEnvironment -> Expr -> GMCode
compileR env e = compileC env e ++ [Update n, Pop n, Unwind] where
    n = length env

-- Compile the expression e by generating code which constructs the
-- graph of e in the environment p leaving a pointer to it on top
-- of the stack
-- C(f)     p = [Pushglobal f]
-- C(x)     p = [Push (p x)]
-- C(i)     p = [Pushint i]
-- C(e1 e2) p = C[e2] p ++ C[e1] p(offset+1) ++ [Mkap]
compileC :: GMEnvironment -> Expr -> GMCode
compileC env (Var v) = case lookup v env of
    Just n  -> [Push n]
    Nothing -> [Pushglobal v]
compileC env (Num n) = [Pushint n]
compileC env (App e1 e2) = compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]
compileC env (Let recursive defs body)
    | recursive = compileLetrec env defs body
    | otherwise = compileLet    env defs body

-- Generate code to construct each let binding and the let body.
-- Code must remove bindings after body is evaluated.
compileLet :: GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLet env defs body = compileDefs env defs ++ compileC env' body ++ [Slide (length defs)] where
    env' = compileArgs env defs

-- Generate code to construct each definition in defs
compileDefs :: GMEnvironment -> [(Name, Expr)] -> GMCode
compileDefs env []                  = []
compileDefs env ((name, expr):defs) = compileC env expr ++ compileDefs (argOffset 1 env) defs

-- Generate code to construct recursive let bindings and the let body.
-- Code must remove bindings after body is evaluated.
-- Bindings start as null pointers and must update themselves on evaluation.
compileLetrec :: GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLetrec env defs body = [Alloc n] ++ compileRecDefs (n - 1) env' defs ++ compileC env' body ++ [Slide n] where
    env' = compileArgs env defs
    n = length defs

-- Generate code to construct each definition in defs and
-- update pointer on stack.
compileRecDefs :: Int -> GMEnvironment -> [(Name, Expr)] -> GMCode
compileRecDefs n env []                  = []
compileRecDefs n env ((name, expr):defs) = compileC env expr ++ [Update n] ++ compileRecDefs (n - 1) (argOffset 1 env) defs

-- Generate stack offsets for local bindings
compileArgs :: GMEnvironment -> [(Name, Expr)] -> GMEnvironment
compileArgs env defs = zip (map fst defs) (reverse [0..n-1]) ++ argOffset n env where
    n = length defs

-- Adjust the stack offsets in the environment by n
argOffset :: Int -> GMEnvironment -> GMEnvironment
argOffset n = map $ second (+n)

-- Move from state to state until final state is reached
eval :: GMState -> [GMState]
eval state = state:rest where
    rest | isFinal state = []
         | otherwise     = eval next
    next = doAdmin (step state)

-- Take a single step from on state to the next.
-- For interactive debugging.
single :: GMState -> GMState
single state = state' where
    state' | isFinal state = state
           | otherwise     = doAdmin (step state)

-- Update machine statistics
doAdmin :: GMState -> GMState
doAdmin state = state { gmStats = stats' } where
    stats  = gmStats state
    stats' = stats { gmSteps = gmSteps stats + 1 }

-- Finished when no more code to execute
isFinal :: GMState -> Bool
isFinal state = null $ gmCode state

-- Transition to next state
step :: GMState -> GMState
step state = dispatch x state' where
    (x:xs) = gmCode state
    state' = state { gmCode = xs }

-- Dispatch from instruction to implementation
dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Pop n)        = pop n
dispatch (Update n)     = update n
dispatch (Alloc n)      = alloc n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

-- Find global node by name
-- (Pushglobal f : i, s,     h, m[(f, a)])
-- (i,                a : s, h, m)
pushglobal :: Name -> GMState -> GMState
pushglobal f state = state { gmStack = addr:gmStack state } where
    addr = case lookup f (gmGlobals state) of
        Just x  -> x
        Nothing -> error ("Undeclared global " ++ f)

-- Allocate number in heap and push on stack
-- (Pushint n : i, s,     h,              m)
-- (i,             a : s, h[(a, NNum n)], m)
pushint :: Int -> GMState -> GMState
pushint n state = state { gmStack = addr:gmStack state, gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NNum n

-- Push address of argument on stack
-- (Push n : i, a0 : ... : an+1 : s,         h[(an + 1, NApp an an')], m)
-- (i,          an' : a0 : ... : an + 1 : s, h,                        m)
push :: Int -> GMState -> GMState
push n state = state { gmStack = addr:stack } where
    stack = gmStack state
    addr  = stack !! n

-- Pop n items from the stack
-- (Pop n : i, a1 : ... : an : s, h, m)
-- (i,         s,                 h, m)
pop :: Int -> GMState -> GMState
pop n state =  state { gmStack = stack' } where
    stack' = drop n $ gmStack state

--- Remove items from stack leaving top-of-stack
--- (Slide n : i, a0 : ... : an : s, h, m)
--- (i,           a0 : s,            h, m)
slide :: Int -> GMState -> GMState
slide n state = state { gmStack = stack' } where
    (x:xs) = gmStack state
    stack' = x:drop n xs

-- Allocate n nodes in the heap and put their addresses on the stack
-- (Alloc n : i, s,                 h,                                                  m)
-- (i,           a1 : ... : an : s, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
alloc :: Int -> GMState -> GMState
alloc n state = state { gmStack = stack', gmHeap = heap' } where
    (heap', addrs) = allocNodes n $ gmHeap state
    stack' = addrs ++ gmStack state

-- Allocate n nodes in the heap. Return new heap and list of addresses.
allocNodes :: Int -> GMHeap -> (GMHeap, [Addr])
allocNodes n heap
    | n < 1     = (heap, [])
    | otherwise = (heap'', addr:addrs) where
        (heap', addrs) = allocNodes (n - 1) heap
        (heap'', addr) = hAlloc heap' (NPointer hNull)

-- Build application from 2 addresses on top of stack
-- (Mkap : i, a1 : a2 : s, h,                  m)
-- (i,        a : s,       h[(a, NApp a1 a2)], m)
mkap :: GMState -> GMState
mkap state = state { gmStack = addr:stack', gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NApp a1 a2
    (a1:a2:stack') = gmStack state

-- Replace root of redex with pointer to top-of-stack
-- (Update n : i, a : a0 : ... : an : s, h,                  m)
-- (i,            a0 : ... : an : s,     h[(an, NPointer a), m)
update :: Int -> GMState -> GMState
update n state = state { gmStack = stack', gmHeap = heap' } where
    (addr:stack') = gmStack state
    heap' = hUpdate (gmHeap state) (stack' !! n) $ NPointer addr

-- Use top of stack to build next state
unwind :: GMState -> GMState
unwind state = newState $ hLoad heap x where
    (x:xs) = gmStack state
    heap = gmHeap state

    -- Number on stack; G-Machine is terminating.
    -- ([Unwind], a : s, h[(a, NNum)], m)
    -- ([],       a : s, h,            m)
    newState (NNum _) = state { gmCode = [] }

    -- Application; keep unwinding applications onto stack
    -- ([Unwind], a : s,      h[(a, NApp a1 a2)], m)
    -- ([Unwind], a1 : a : s, h,                  m)
    newState (NApp a1 a2) = state { gmCode = [Unwind], gmStack = (a1:x:xs) }

    -- Pointer; dereference and replace top-of-stack
    -- ([Unwind], a0 : s, h[(a0, NPointer a)], m)
    -- ([Unwind], a : s,  h,                   m)
    newState (NPointer a) = state { gmCode = [Unwind], gmStack = a:xs }

    -- Global; put code for global in code component of machine.
    -- ([Unwind], a0 : ... : an : s,   h[(a0, NGlobal n c), (NApp a0 a1'), ..., (NApp an-1, an')], m)
    -- (c,        a1' : ... : an' : s, h,                                                          m)
    newState (NGlobal n code)
        | length xs < n = error "Unwinding with too few arguments"
        | otherwise     = state { gmCode = code, gmStack = rearrange n heap (x:xs) }

-- Pull n arguments directly onto the stack out of NApp nodes
rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap stack = take n stack' ++ drop n stack where
    stack' = map (getArg . hLoad heap) $ tail stack

-- Get argument component from application
getArg :: Node -> Addr
getArg (NApp a1 a2) = a2
getArg _            = error "Attempted to load argument to non-application node"

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

-- Format list of instructions
formatCode :: GMCode -> Doc
formatCode code = text "Code" <> colon $$ nest 4 (vcat $ map (text . show) code)

-- Format stack and current code
formatState :: (GMState, Int) -> Doc
formatState (state, n) = text "State" <+> int n <> colon $$ nest 4 (formatStack state $$ formatCode (gmCode state))

-- Format nodes on stack
formatStack :: GMState -> Doc
formatStack state = text "Stack" <> colon $$ nest 4 (vcat $ map (formatNode state) (reverse (gmStack state)))

-- Format a single node
formatNode :: GMState -> Addr -> Doc
formatNode state addr = formatAddr addr <> colon <+> draw (hLoad (gmHeap state) addr) where
    draw (NNum n) = int n
    draw (NGlobal n g) = text "Global" <+> text v where
        (v, _) = head (filter (\(x, b) -> b == addr) (gmGlobals state))
    draw (NApp a1 a2) = text "App" <+> formatAddr a1 <+> formatAddr a2
    draw (NPointer a) = text "Pointer to" <+> formatAddr a

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


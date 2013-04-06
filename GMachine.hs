module GMachine where

import Heap
import Expr
import Parse (parseCore)

import Control.Arrow hiding ((<+>))
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
                 | Eval                 -- Evaluate top-of-stack to Weak Head Normal Form
                 | Cond GMCode GMCode   -- Condition instruction
                 | Unwind               -- Unwind application nodes onto stack
                 | Add                  -- Arithmetic instructions
                 | Sub
                 | Mul
                 | Div
                 | Neg
                 | Eq                   -- Relational instructions
                 | Ne
                 | Lt
                 | Le
                 | Gt
                 | Ge
                   deriving Show

-- Executions stack
type GMStack = [Addr]

-- Save machine's current context during
-- strict evaluation
type GMDump = [(GMCode, GMStack)]

-- Heap of live objects
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
    gmDump    :: GMDump,
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
run = show . formatResults . evaluate . compile . parseCore

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
compile program = GMState codeInit [] [] heap globals statInit where
    (heap, globals) = buildInitialHeap program

-- Push main and unwind
codeInit :: GMCode
codeInit = [Pushglobal "main", Eval]

-- Start at step zero
statInit :: GMStats
statInit = GMStats 0

-- Compiles primitives
prims :: [(Name, Int, GMCode)]
prims =
    [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
     ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
     ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
     ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
     ("negate", 1, [Push 1, Eval, Neg, Update 1, Pop 1, Unwind]),
     ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
     ("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
     ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
     ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
     (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
     (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
     ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])]

-- Instantiate supercombinators in heap
buildInitialHeap :: Program -> (GMHeap, GMGlobals)
buildInitialHeap program = mapAccumL allocSC hInit compiled where
    compiled = map compileSC (prelude ++ extraDefs ++ program) ++ prims

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
evaluate :: GMState -> [GMState]
evaluate state = state:rest where
    rest | isFinal state = []
         | otherwise     = evaluate next
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
dispatch (Push n)       = push n
dispatch (Pop n)        = pop n
dispatch (Slide n)      = slide n
dispatch (Alloc n)      = alloc n
dispatch (Update n)     = update n
dispatch (Cond t f)     = cond t f
dispatch Mkap           = mkap
dispatch Eval           = eval
dispatch Unwind         = unwind
dispatch Add            = arithBinary (+)
dispatch Sub            = arithBinary (-)
dispatch Mul            = arithBinary (*)
dispatch Div            = arithBinary div
dispatch Neg            = arithUnary negate
dispatch Eq             = compBinary (==)
dispatch Ne             = compBinary (/=)
dispatch Lt             = compBinary (<)
dispatch Le             = compBinary (<=)
dispatch Gt             = compBinary (>)
dispatch Ge             = compBinary (>=)

-- Find global node by name
-- (Pushglobal f : i, s,     d, h, m[(f, a)])
-- (i,                a : s, d, h, m)
pushglobal :: Name -> GMState -> GMState
pushglobal f state = state { gmStack = addr:gmStack state } where
    addr = case lookup f (gmGlobals state) of
        Just x  -> x
        Nothing -> error ("Undeclared global " ++ f)

-- Allocate number in heap and push on stack
-- (Pushint n : i, s,     d, h,              m)
-- (i,             a : s, d, h[(a, NNum n)], m)
pushint :: Int -> GMState -> GMState
pushint n state = state { gmStack = addr:gmStack state, gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NNum n

-- Push address of argument on stack
-- (Push n : i, a0 : ... : an+1 : s,         d, h[(an + 1, NApp an an')], m)
-- (i,          an' : a0 : ... : an + 1 : s, d, h,                        m)
push :: Int -> GMState -> GMState
push n state = state { gmStack = addr:stack } where
    stack = gmStack state
    addr  = stack !! n

-- Pop n items from the stack
-- (Pop n : i, a1 : ... : an : s, d, h, m)
-- (i,         s,                 d, h, m)
pop :: Int -> GMState -> GMState
pop n state =  state { gmStack = stack' } where
    stack' = drop n $ gmStack state

--- Remove items from stack leaving top-of-stack
--- (Slide n : i, a0 : ... : an : s, d, h, m)
--- (i,           a0 : s,            d, h, m)
slide :: Int -> GMState -> GMState
slide n state = state { gmStack = stack' } where
    (x:xs) = gmStack state
    stack' = x:drop n xs

-- Allocate n nodes in the heap and put their addresses on the stack
-- (Alloc n : i, s,                 d, h,                                                  m)
-- (i,           a1 : ... : an : s, d, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
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

-- Replace root of redex with pointer to top-of-stack
-- (Update n : i, a : a0 : ... : an : s, d, h,                  m)
-- (i,            a0 : ... : an : s,     d, h[(an, NPointer a), m)
update :: Int -> GMState -> GMState
update n state = state { gmStack = stack', gmHeap = heap' } where
    (addr:stack') = gmStack state
    heap' = hUpdate (gmHeap state) (stack' !! n) $ NPointer addr

-- Choose first branch if top-of-stack is true (1)
-- (Cond t f : i, a : s, d, h[(a, NNum 1)], m)
-- (t : i,        s,     d, h,              m)
-- Choose second branch if top-of-stack if false (0)
-- (Cond t f : i, a : s, d, h[(a, NNum 0)], m)
-- (f : i,        s,     d, h,              m)
cond :: GMCode -> GMCode -> GMState -> GMState
cond consequent alternative state = state { gmCode = code, gmStack = stack } where
    (addr:stack) = gmStack state
    code = case hLoad (gmHeap state) addr of
        (NNum 1) -> consequent ++ gmCode state
        (NNum 0) -> alternative ++ gmCode state
        node     -> error $ "Non-Boolean " ++ show node ++ " used in conditional"

-- Build application from 2 addresses on top of stack
-- (Mkap : i, a1 : a2 : s, d, h,                  m)
-- (i,        a : s,       d, h[(a, NApp a1 a2)], m)
mkap :: GMState -> GMState
mkap state = state { gmStack = addr:stack', gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NApp a1 a2
    (a1:a2:stack') = gmStack state

-- Put bottom of stack and instructions on dump.
-- Leave top-of-stack as only element on stack and unwind.
-- (Eval : i, a : s,          d, h, m)
-- ([Unwind], [a],   (i, s) : d, h, m)
eval :: GMState -> GMState
eval state =  state { gmCode = [Unwind], gmStack = [addr], gmDump = (code, stack):dump } where
    (addr:stack) = gmStack state
    code = gmCode state
    dump = gmDump state

-- Use top of stack to build next state
unwind :: GMState -> GMState
unwind state = newState $ hLoad heap x where
    (x:xs) = gmStack state
    heap = gmHeap state
    dump = gmDump state

    -- Number on stack and empty dump; G-Machine is terminating.
    -- ([Unwind], a : s, [], h[(a, NNum)], m)
    -- ([],       a : s, [], h,            m)
    -- Or number on stack and not-empty dump; restore code and stack
    -- ([Unwind], a : s,  (c, s') : d, h[(a, NNum)], m)
    -- (c,        a : s', d,           h,            m)
    newState (NNum _) = case dump of
        (code', stack'):dump' -> state { gmCode = code', gmStack = x:stack', gmDump = dump' }
        _                     -> state { gmCode = [] }

    -- Application; keep unwinding applications onto stack
    -- ([Unwind], a : s,      d, h[(a, NApp a1 a2)], m)
    -- ([Unwind], a1 : a : s, d, h,                  m)
    newState (NApp a1 a2) = state { gmCode = [Unwind], gmStack = (a1:x:xs) }

    -- Pointer; dereference and replace top-of-stack
    -- ([Unwind], a0 : s, d, h[(a0, NPointer a)], m)
    -- ([Unwind], a : s,  d, h,                   m)
    newState (NPointer a) = state { gmCode = [Unwind], gmStack = a:xs }

    -- Global; put code for global in code component of machine.
    -- ([Unwind], a0 : ... : an : s,   d, h[(a0, NGlobal n c), (NApp a0 a1'), ..., (NApp an-1, an')], m)
    -- (c,        a1' : ... : an' : s, d, h,                                                          m)
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

-- Box integer as NNum node in heap and put on top-of-stack
boxInt :: Int -> GMState -> GMState
boxInt n state = state { gmStack = addr:stack, gmHeap = heap } where
    (heap, addr) = hAlloc (gmHeap state) $ NNum n
    stack = gmStack state

-- Load integer from heap
unboxInt :: Addr -> GMState -> Int
unboxInt addr state = case hLoad (gmHeap state) addr of
    (NNum n) -> n
    node     -> error $ "Attempted to unbox integer from " ++ show node

-- Box Boolean as NNum node in heap and put on top-of-stack
boxBool :: Bool -> GMState -> GMState
boxBool b state = state { gmStack = addr:stack, gmHeap = heap } where
    (heap, addr) = hAlloc (gmHeap state) $ NNum b'
    stack = gmStack state
    b' | b         = 1
       | otherwise = 0

-- Generate a state transition performing a primitive unary operation
primUnary :: (b -> GMState -> GMState) -- boxing function
          -> (Addr -> GMState -> a)    -- unboxing function
          -> (a -> b)                  -- unary operator
          -> (GMState -> GMState)      -- state transition
primUnary box unbox op state = box (op (unbox addr state)) state' where
    addr:stack = gmStack state
    state' = state { gmStack = stack }

-- Generate a state transition performing a primitive binary operation
primBinary :: (b -> GMState -> GMState) -- boxing function
          -> (Addr -> GMState -> a)    -- unboxing function
          -> (a -> a -> b)             -- binary operator
          -> (GMState -> GMState)      -- state transition
primBinary box unbox op state = box (op (unbox x state) (unbox y state)) state' where
    x:y:stack = gmStack state
    state' = state { gmStack = stack }

-- Generate a state transistion performing an arithmetic unary operation
arithUnary :: (Int -> Int) -> (GMState -> GMState)
arithUnary = primUnary boxInt unboxInt

-- Generate a state transistion performing an arithmetic binary operation
arithBinary :: (Int -> Int -> Int) -> (GMState -> GMState)
arithBinary = primBinary boxInt unboxInt

-- Generate a state transistion performing a relational binary operation
compBinary :: (Int -> Int -> Bool) -> (GMState -> GMState)
compBinary = primBinary boxBool unboxInt

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
formatState (state, n) = text "State" <+> int n <> colon $$ nest 4 (formatStack state $$ formatCode (gmCode state) $$ formatDump state)

-- Format nodes on stack
formatStack :: GMState -> Doc
formatStack state = text "Stack" <> colon $$ nest 4 (vcat $ map (formatNode state) (reverse (gmStack state)))

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

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


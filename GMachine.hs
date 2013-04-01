module GMachine where

import Heap
import Expr
import Parse (parseCore)

-- Code is just a list of instructions
type GMCode = [Instruction]
data Instruction = Unwind
                 | Pushglobal Name
                 | Pushint Int
                 | Push Int
                 | Mkap
                 | Slide Int
                   deriving Show

type GMStack = [Addr]
type GMHeap = Heap Node

-- Heap data
data Node = NNum Int
          | NApp Addr Addr
          | NGlobal Int GMCode
            deriving Show

-- Global environment maps names to addresses
type GMGlobals = [(Name, Addr)]

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

-- Move from state to state until final state is reached
eval :: GMState -> [GMState]
eval state = state:rest where
    rest | isFinal state = []
         | otherwise     = eval next
    next = doAdmin (step state)

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
    (heap', addr) = alloc (gmHeap state) $ NNum n

-- Build application from 2 addresses on top of stack
-- (Mkap : i, a1 : a2 : s, h,                  m)
-- (i,        a : s,       h[(a, NApp a1 a2)], m)
mkap :: GMState -> GMState
mkap state = state { gmStack = addr:stack', gmHeap = heap' } where
    (heap', addr) = alloc (gmHeap state) $ NApp a1 a2
    (a1:a2:stack') = gmStack state

-- Push address of argument on stack
-- (Push n : i, a0 : ... : an+1 : s,         h[(an + 1, NApp an an')], m)
-- (i,          an' : a0 : ... : an + 1 : s, h,                        m)
push :: Int -> GMState -> GMState
push n state = state { gmStack = addr:stack } where
    stack = gmStack state
    addr  = getArg $ load (gmHeap state) $ stack !! (n + 1)

-- Get argument component from application
getArg :: Node -> Addr
getArg (NApp a1 a2) = a2
getArg _            = error "Attempted to load argument to non-application node"

-- Remove information about previous application from stack
-- (Slide n : i, a0 : ... : an : s, h, m)
-- (i,           a0 : s,            h, m)
slide :: Int -> GMState -> GMState
slide n state = state { gmStack = stack' } where
    (x:xs) = gmStack state
    stack' = x:drop n xs

-- Use top of stack to build next state
unwind :: GMState -> GMState
unwind state = newState $ load heap x where
    (x:xs) = gmStack state
    heap = gmHeap state

    -- Number on stack; G-Machine is terminating.
    -- ([Unwind], a : s, h[(a, NNum)], m)
    -- ([],       a : s, h,            m)
    newState (NNum _) = state { gmCode = [] }

    -- Application; keep unwinding applications onto stack
    -- ([Unwind], a : s,      h[(a, NApp a1 a2)], m)
    -- ([Unwind], a1 : a : s, h,                  m)
    newState (NApp a1 a2) = state { gmCode = [Unwind], gmStack = (a1:a2:xs) }

    -- Global; put code for global in code component of machine.
    -- ([Unwind], a0 : ... : an : s, h[(a0, NGlobal n c)], m)
    -- (c,        a0 : ... : an : s, h,                    m)
    newState (NGlobal n code)
        | length xs < n = error "Unwinding with too few arguments"
        | otherwise     = state { gmCode = code }


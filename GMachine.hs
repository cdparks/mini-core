module GMachine (
    evaluate,
    single
) where

import Types
import Heap

import Data.List
import Debug.Trace

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
dispatch (Pushglobal f)   = pushglobal f
dispatch (Pushcons f t n) = pushcons f t n
dispatch (Pushint n)      = pushint n
dispatch (Push n)         = push n
dispatch (Pop n)          = pop n
dispatch (Slide n)        = slide n
dispatch (Alloc n)        = alloc n
dispatch (Update n)       = update n
dispatch (Cond t f)       = cond t f
dispatch (Pack t n)       = pack t n
dispatch (Casejump alts)  = casejump alts
dispatch (Split n)        = split n
dispatch Mkap             = mkap
dispatch Eval             = eval
dispatch Unwind           = unwind
dispatch Print            = print'
dispatch Add              = arithBinary (+)
dispatch Sub              = arithBinary (-)
dispatch Mul              = arithBinary (*)
dispatch Div              = arithBinary div
dispatch Neg              = arithUnary negate
dispatch Eq               = compBinary (==)
dispatch Ne               = compBinary (/=)
dispatch Lt               = compBinary (<)
dispatch Le               = compBinary (<=)
dispatch Gt               = compBinary (>)
dispatch Ge               = compBinary (>=)
--dispatch i               = error $ "instruction " ++ show i ++ " not implemented yet"

-- Find global node by name
-- (o, Pushglobal f : i, s,     d, h, m[(f, a)])
-- (o, i,                a : s, d, h, m)
pushglobal :: Name -> GMState -> GMState
pushglobal f state = state { gmStack = addr:gmStack state } where
    addr = case lookup f (gmGlobals state) of
        Just x  -> x
        Nothing -> error ("Undeclared global " ++ f)

-- Find global node for constructor by name or add to globals
pushcons :: Name -> Int -> Int -> GMState -> GMState
pushcons name tag arity state = case lookup name (gmGlobals state) of
    Just addr -> state { gmStack = addr:gmStack state }
    Nothing   -> state { gmStack = addr:gmStack state, gmHeap = heap, gmGlobals = globals } where
        (heap, addr) = hAlloc (gmHeap state) $ NGlobal arity [Pack tag arity, Update 0, Unwind]
        globals = (name, addr):gmGlobals state

-- Allocate number in heap and push on stack
-- (o, Pushint n : i, s,     d, h,              m)
-- (o, i,             a : s, d, h[(a, NNum n)], m)
pushint :: Int -> GMState -> GMState
pushint n state = state { gmStack = addr:gmStack state, gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NNum n

-- Push address of argument on stack
-- (o, Push n : i, a0 : ... : an+1 : s,         d, h[(an + 1, NApp an an')], m)
-- (o, i,          an' : a0 : ... : an + 1 : s, d, h,                        m)
push :: Int -> GMState -> GMState
push n state = state { gmStack = addr:stack } where
    stack = gmStack state
    addr  = stack !! n

-- Pop n items from the stack
-- (o, Pop n : i, a1 : ... : an : s, d, h, m)
-- (o, i,         s,                 d, h, m)
pop :: Int -> GMState -> GMState
pop n state =  state { gmStack = stack' } where
    stack' = drop n $ gmStack state

-- Remove items from stack leaving top-of-stack
-- (o, Slide n : i, a0 : ... : an : s, d, h, m)
-- (o, i,           a0 : s,            d, h, m)
slide :: Int -> GMState -> GMState
slide n state = state { gmStack = stack' } where
    (x:xs) = gmStack state
    stack' = x:drop n xs

-- Allocate n nodes in the heap and put their addresses on the stack
-- (o, Alloc n : i, s,                 d, h,                                                  m)
-- (o, i,           a1 : ... : an : s, d, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
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
-- (o, Update n : i, a : a0 : ... : an : s, d, h,                  m)
-- (o, i,            a0 : ... : an : s,     d, h[(an, NPointer a), m)
update :: Int -> GMState -> GMState
update n state = state { gmStack = stack', gmHeap = heap' } where
    (addr:stack') = gmStack state
    heap' = hUpdate (gmHeap state) (stack' !! n) $ NPointer addr

-- Choose first branch if top-of-stack is true (1)
-- (o, Cond t f : i, a : s, d, h[(a, NNum 1)], m)
-- (o, t : i,        s,     d, h,              m)
-- Choose second branch if top-of-stack if false (0)
-- (o, Cond t f : i, a : s, d, h[(a, NNum 0)], m)
-- (o, f : i,        s,     d, h,              m)
cond :: GMCode -> GMCode -> GMState -> GMState
cond consequent alternative state = state { gmCode = code, gmStack = stack } where
    (addr:stack) = gmStack state
    code = case hLoad (gmHeap state) addr of
        (NNum 1) -> consequent ++ gmCode state
        (NNum 0) -> alternative ++ gmCode state
        node     -> error $ "Non-Boolean " ++ show node ++ " used in conditional"

-- Build constructor node in heap from stack elements
-- (o, Pack t n : i, a1 : ... : an : s, d, h,                                    m)
-- (o, i,            a : s,             d, h[(a, NConstructor t [a1, ..., an])], m)
pack :: Int -> Int -> GMState -> GMState
pack tag arity state = state { gmStack = addr:stack', gmHeap = heap' } where
    stack = gmStack state
    (args, stack') | length stack < arity = error "Not enough arguments to saturate constructor"
                   | otherwise            = splitAt arity stack
    (heap', addr) = hAlloc (gmHeap state) $ NConstructor tag args

-- Evaluate top-of-stack to WHNF and use tag to jump to code
-- (o, Casejump [..., t -> i', ...] : i, a : s, d, h[(a, NConstructor t cs)], m)
-- (o, i' ++ i,                          a : s, d, h,                         m)
casejump :: [(Int, GMCode)] -> GMState -> GMState
casejump alts state = state { gmCode = branch ++ code } where
    addr:_ = gmStack state
    (NConstructor tag _) = hLoad (gmHeap state) addr
    code = gmCode state
    branch = case lookup tag alts of
        Just branch -> branch
        Nothing     -> error "Non-exhaustive patterns in case expression"

-- Destructure constructor onto stack
-- (o, Split n : i, a : s,             d, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i,           a1 : ... : an : s, d, h,                                    m)
split :: Int -> GMState -> GMState
split n state = state { gmStack = addrs ++ stack } where
    addr:stack = gmStack state
    (NConstructor _ args) = hLoad (gmHeap state) addr
    addrs | length args == n = args
          | otherwise        = error $ "Cannot destructure constructor into " ++ show n ++ " components"

-- Print numbers and constructor components by adding values to output list
-- (o,      Print : i, a : s, d, h[(a, NNum n)], m)
-- (o ++ n, i,         s,     d, h,              m)
-- Or
-- (o, Print : i, a : s,                 d, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i' ++ i,   a1 : ... : an : s,     d, h,                                    m)
-- where i' = concat (take n (repeat [Eval, Print]))
print' :: GMState -> GMState
print' state = doPrint $ hLoad (gmHeap state) addr where
    addr:stack = gmStack state
    printN n = concat $ take n $ repeat [Eval, Print]
    doPrint (NNum n)              = state { gmOutput = show n:gmOutput state, gmStack = stack }
    doPrint (NConstructor _ args) = state { gmCode = printN (length args) ++ gmCode state, gmStack = args ++ stack }
    doPrint node                  = error $ "Can't print node " ++ show node

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
    -- ([Unwind], a : s, [], h[(a, NNum n)], m)
    -- ([],       a : s, [], h,            m)
    -- Or number on stack and not-empty dump; restore code and stack
    -- ([Unwind], a : s,  (c, s') : d, h[(a, NNum n)], m)
    -- (c,        a : s', d,           h,            m)
    newState (NNum _) = restoreDump x state

    -- Constructor on stack and empty dump; G-Machine is terminating.
    -- ([Unwind], a : s, [], h[(a, Constructor tar args)], m)
    -- ([],       a : s, [], h,            m)
    -- Or Constructor on stack and not-empty dump; restore code and stack
    -- ([Unwind], a : s,  (c, s') : d, h[(a, Constructor tag args)], m)
    -- (c,        a : s', d,           h,            m)
    newState (NConstructor tag args) = restoreDump x state

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
    -- If we're evaluating something to WHNF, there will be information on the
    -- dump. In this case, we don't need to fully apply the combinator, and if we
    -- can't, we should just return the root of the redex:
    -- ([Unwind], [a0, ..., ak], (i, s) : d, h[(a0, NGlobal n c)], m)
    -- (i,        ak : s,                 d, h,                    m) when k < n
    newState (NGlobal n code)
        | length xs >= n = state { gmCode = code,  gmStack = rearrange n heap (x:xs) }
        | otherwise      = case dump of
            (code', stack'):dump' -> state { gmCode = code', gmStack = last (x:xs):stack' }
            _                     -> error "Unwinding with too few arguments"

-- If dump is not empty, restore to machine state. Otherwise,
-- halt execution.
restoreDump :: Addr -> GMState -> GMState
restoreDump addr state = case gmDump state of
    (code, stack):dump -> state { gmCode = code, gmStack = addr:stack, gmDump = dump }
    _                  -> state { gmCode = [] }

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


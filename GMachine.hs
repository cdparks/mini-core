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

-- Update machine statistics. Collect garbage if heap has grown
-- too large
doAdmin :: GMState -> GMState
doAdmin state = state { gmStats = GMStats steps collections, gmHeap = heap' } where
    heap   = gmHeap state
    heap'  | doCollect = gc state
           | otherwise = heap
    doCollect = hSize heap > hMaxSize heap
    stats = gmStats state
    steps = gmSteps stats + 1
    collections | doCollect = gmCollections stats + 1
                | otherwise = gmCollections stats

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
dispatch (Pushbasic n)    = pushbasic n
dispatch (Push n)         = push n
dispatch (Pop n)          = pop n
dispatch (Slide n)        = slide n
dispatch (Alloc n)        = alloc n
dispatch (Update n)       = update n
dispatch (Pack t n)       = pack t n
dispatch (Casejump alts)  = casejump alts
dispatch (Cond t f)       = cond t f
dispatch (Split n)        = split n
dispatch Mkap             = mkap
dispatch Mkint            = mkint
dispatch Mkbool           = mkbool
dispatch Get              = get
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
-- (o, Pushglobal f : i, s,     d, v, h, m[(f, a)])
-- (o, i,                a : s, d, v, h, m)
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
-- (o, Pushint n : i, s,     d, v, h,              m)
-- (o, i,             a : s, d, v, h[(a, NNum n)], m)
pushint :: Int -> GMState -> GMState
pushint n state = state { gmStack = addr:gmStack state, gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NNum n

-- Push unboxed integer onto V-stack
-- (o, Pushbasic n : i, s, d, v,     h, m)
-- (o, i,               s, d, n : v, h, m)
pushbasic :: Int -> GMState -> GMState
pushbasic n state = state { gmVStack = n:gmVStack state }

-- Push address of argument on stack
-- (o, Push n : i, a0 : ... : an+1 : s,         d, v, h[(an + 1, NApp an an')], m)
-- (o, i,          an' : a0 : ... : an + 1 : s, d, v, h,                        m)
push :: Int -> GMState -> GMState
push n state = state { gmStack = addr:stack } where
    stack = gmStack state
    addr  = stack !! n

-- Pop n items from the stack
-- (o, Pop n : i, a1 : ... : an : s, d, v, h, m)
-- (o, i,         s,                 d, v, h, m)
pop :: Int -> GMState -> GMState
pop n state =  state { gmStack = stack' } where
    stack' = drop n $ gmStack state

-- Remove items from stack leaving top-of-stack
-- (o, Slide n : i, a0 : ... : an : s, d, v, h, m)
-- (o, i,           a0 : s,            d, v, h, m)
slide :: Int -> GMState -> GMState
slide n state = state { gmStack = stack' } where
    (x:xs) = gmStack state
    stack' = x:drop n xs

-- Allocate n nodes in the heap and put their addresses on the stack
-- (o, Alloc n : i, s,                 d, v, h,                                                  m)
-- (o, i,           a1 : ... : an : s, d, v, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
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
-- (o, Update n : i, a : a0 : ... : an : s, d, v, h,                  m)
-- (o, i,            a0 : ... : an : s,     d, v, h[(an, NPointer a), m)
update :: Int -> GMState -> GMState
update n state = state { gmStack = stack', gmHeap = heap' } where
    (addr:stack') = gmStack state
    heap' = hUpdate (gmHeap state) (stack' !! n) $ NPointer addr

-- Build constructor node in heap from stack elements
-- (o, Pack t n : i, a1 : ... : an : s, d, v, h,                                    m)
-- (o, i,            a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
pack :: Int -> Int -> GMState -> GMState
pack tag arity state = state { gmStack = addr:stack', gmHeap = heap' } where
    stack = gmStack state
    (args, stack') | length stack < arity = error "Not enough arguments to saturate constructor"
                   | otherwise            = splitAt arity stack
    (heap', addr) = hAlloc (gmHeap state) $ NConstructor tag args

-- Evaluate top-of-stack to WHNF and use tag to jump to code
-- (o, Casejump [..., t -> i', ...] : i, a : s, d, v, h[(a, NConstructor t cs)], m)
-- (o, i' ++ i,                          a : s, d, v, h,                         m)
casejump :: [(Int, GMCode)] -> GMState -> GMState
casejump alts state = state { gmCode = branch ++ code } where
    addr:_ = gmStack state
    (NConstructor tag _) = hLoad (gmHeap state) addr
    code = gmCode state
    branch = case lookup tag alts of
        Just branch -> branch
        Nothing     -> error "Non-exhaustive patterns in case expression"

-- Simple branch using top-of-V-stack
-- If top-of-V-stack is 2 (True tag):
-- (o, Cond t f : i, s, d, 2 : v, h, m)
-- (o, t ++ i,       s, d, v,     h, m)
-- If top-of-V-stack is 1 (False tag):
-- (o, Cond t f : i, s, d, 1 : v, h, m)
-- (o, f ++ i,       s, d, v,     h, m)
cond :: GMCode -> GMCode -> GMState -> GMState
cond consequent alternative state = state { gmCode = branch ++ gmCode state, gmVStack = vstack } where
    b:vstack = gmVStack state
    branch = case b of
        2 -> consequent
        1 -> alternative
        x -> error $ "Non-Boolean " ++ show x ++ " used in Boolean context"

-- Destructure constructor onto stack
-- (o, Split n : i, a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i,           a1 : ... : an : s, d, v, h,                                    m)
split :: Int -> GMState -> GMState
split n state = state { gmStack = addrs ++ stack } where
    addr:stack = gmStack state
    (NConstructor _ args) = hLoad (gmHeap state) addr
    addrs | length args == n = args
          | otherwise        = error $ "Cannot destructure constructor into " ++ show n ++ " components"

-- Print numbers and constructor components by adding values to output list
-- (o,      Print : i, a : s, d, v, h[(a, NNum n)], m)
-- (o ++ n, i,         s,     d, v, h,              m)
-- Or
-- (o, Print : i, a : s,                 d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i' ++ i,   a1 : ... : an : s,     d, v, h,                                    m)
-- where i' = concat (take n (repeat [Eval, Print]))
print' :: GMState -> GMState
print' state = doPrint $ hLoad (gmHeap state) addr where
    addr:stack = gmStack state
    printN n = concat $ take n $ repeat [Eval, Print]
    doPrint (NNum n)              = state { gmOutput = show n:gmOutput state, gmStack = stack }
    doPrint (NConstructor _ args) = state { gmCode = printN (length args) ++ gmCode state, gmStack = args ++ stack }
    doPrint node                  = error $ "Can't print node " ++ show node

-- Build application from 2 addresses on top of stack
-- (o, Mkap : i, a1 : a2 : s, d, v, h,                  m)
-- (o, i,        a : s,       d, v, h[(a, NApp a1 a2)], m)
mkap :: GMState -> GMState
mkap state = state { gmStack = addr:stack', gmHeap = heap' } where
    (heap', addr) = hAlloc (gmHeap state) $ NApp a1 a2
    (a1:a2:stack') = gmStack state

-- Box top-of-V-stack into heap as integer, put address on top-of-stack
-- (o, Mkint : i, s,     d, n : v, h,              m)
-- (o, i,         a : s, d, v,     h[(a, NNum n)], m)
mkint :: GMState -> GMState
mkint state = state { gmStack = addr:gmStack state, gmHeap = heap, gmVStack = vstack } where
    n:vstack = gmVStack state
    (heap, addr) = hAlloc (gmHeap state) $ NNum n

-- Box top-of-V-stack into heap as Boolean, put address on top-of-stack
-- (o, Mkbool : i, s,     d, b : v, h,                         m)
-- (o, i,          a : s, d, v,     h[(a, NConstructor b [])], m)
mkbool :: GMState -> GMState
mkbool state = state { gmStack = addr:gmStack state, gmHeap = heap, gmVStack = vstack } where
    b:vstack = gmVStack state
    (heap, addr) = hAlloc (gmHeap state) $ NConstructor b []

-- Unbox top-of-stack and put on V-stack
-- (o, Get : i, a : s, d, v,     h[(a, NNum n)], m)
-- (o, i,       s,     d, n : v, h,              m)
-- or
-- (o, Get : i, a : s, d, v,     h[(a, NConstructor b [])], m)
-- (o, i,       s,     d, b : v, h,                         m)
get :: GMState -> GMState
get state = state { gmStack = stack, gmVStack = x:gmVStack state } where
    addr:stack = gmStack state
    x = case hLoad (gmHeap state) addr of
        (NNum n)            -> n
        (NConstructor b []) -> b
        node                -> error $ "Cannot put node " ++ show node ++ " on V-stack"

-- Put bottom of stack, V-stack, and instructions on dump.
-- Leave top-of-stack as only element on stack and unwind.
-- (o, Eval : i, a : s, d,            v,  h, m)
-- (o, [Unwind], [a],   (i, s, v : d, [], h, m)
eval :: GMState -> GMState
eval state =  state { gmCode = [Unwind], gmStack = [addr], gmVStack = [], gmDump = (code, stack, vstack):dump } where
    (addr:stack) = gmStack state
    code = gmCode state
    vstack = gmVStack state
    dump = gmDump state

-- Use top of stack to build next state
unwind :: GMState -> GMState
unwind state = newState $ hLoad heap x where
    (x:xs) = gmStack state
    heap = gmHeap state
    dump = gmDump state

    -- Number on stack and empty dump; G-Machine is terminating.
    -- (o, [Unwind], a : s, [], v, h[(a, NNum n)], m)
    -- (o, [],       a : s, [], v, h,            m)
    -- Or number on stack and not-empty dump; restore code and stack
    -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, NNum n)], m)
    -- (o, c,        a : s', d,               v', h,              m)
    newState (NNum _) = restoreDump x state

    -- Constructor on stack and empty dump; G-Machine is terminating.
    -- (o, [Unwind], a : s, [], v, h[(a, Constructor tar args)], m)
    -- (o, [],       a : s, [], v, h,            m)
    -- Or Constructor on stack and not-empty dump; restore code and stack
    -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, Constructor tag args)], m)
    -- (o, c,        a : s', d,               v', h,                            m)
    newState (NConstructor tag args) = restoreDump x state

    -- Application; keep unwinding applications onto stack
    -- (o, [Unwind], a : s,      d, v, h[(a, NApp a1 a2)], m)
    -- (o, [Unwind], a1 : a : s, d, v, h,                  m)
    newState (NApp a1 a2) = state { gmCode = [Unwind], gmStack = (a1:x:xs) }

    -- Pointer; dereference and replace top-of-stack
    -- (o, [Unwind], a0 : s, d, v, h[(a0, NPointer a)], m)
    -- (o, [Unwind], a : s,  d, v, h,                   m)
    newState (NPointer a) = state { gmCode = [Unwind], gmStack = a:xs }

    -- Global; put code for global in code component of machine.
    -- (o, [Unwind], a0 : ... : an : s,   d, v, h[(a0, NGlobal n c), (NApp a0 a1'), ..., (NApp an-1, an')], m)
    -- (o, c,        a1' : ... : an' : s, d, v, h,                                                          m)
    -- If we're evaluating something to WHNF, there will be information on the
    -- dump. In this case, we don't need to fully apply the combinator, and if we
    -- can't, we should just return the root of the redex:
    -- (o, [Unwind], [a0, ..., ak], (i, s) : d, v, h[(a0, NGlobal n c)], m)
    -- (o, i,        ak : s,                 d, v, h,                    m) when k < n
    newState (NGlobal n code)
        | length xs >= n = state { gmCode = code,  gmStack = rearrange n heap (x:xs) }
        | otherwise      = case dump of
            (code', stack', vstack):dump' -> state { gmCode = code', gmStack = last (x:xs):stack', gmVStack = vstack }
            _                     -> error "Unwinding with too few arguments"

-- If dump is not empty, restore to machine state. Otherwise,
-- halt execution.
restoreDump :: Addr -> GMState -> GMState
restoreDump addr state = case gmDump state of
    (code, stack, vstack):dump -> state { gmCode = code, gmStack = addr:stack, gmVStack = vstack, gmDump = dump }
    _                          -> state { gmCode = [] }

-- Pull n arguments directly onto the stack out of NApp nodes
rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap stack = take n stack' ++ drop n stack where
    stack' = map (getArg . hLoad heap) $ tail stack

-- Get argument component from application
getArg :: Node -> Addr
getArg (NApp a1 a2) = a2
getArg _            = error "Attempted to load argument to non-application node"

-- Generate a state transition from a unary arithmetic function
arithUnary :: (Int -> Int) -> (GMState -> GMState)
arithUnary op state = state { gmVStack = vstack' } where
    x:vstack = gmVStack state
    vstack' = op x:vstack

-- Generate a state transition from a binary arithmetic function
arithBinary :: (Int -> Int -> Int) -> (GMState -> GMState)
arithBinary op state = state { gmVStack = vstack' } where
    x:y:vstack = gmVStack state
    vstack' = op x y:vstack

-- Generate a state transition from a binary comparison function
compBinary :: (Int -> Int -> Bool) -> (GMState -> GMState)
compBinary op state = state { gmVStack = vstack' } where
    x:y:vstack = gmVStack state
    vstack' | op x y    = 2:vstack  -- True tag
            | otherwise = 1:vstack  -- False tag

-- Simple mark and scan garbage collection
gc :: GMState -> GMHeap
gc state = hIncreaseMax . scanHeap . foldr markFrom (gmHeap state) $ findRoots state

-- Get root addresses from machine state
findRoots :: GMState -> [Addr]
findRoots state = dumpRoots ++ stackRoots ++ globalRoots where
    stackRoots = gmStack state
    globalRoots = map snd $ gmGlobals state
    dumpRoots = concatMap (\(_, stack, _) -> stack) $ gmDump state

-- Start from address and mark all nodes reachable from it
markFrom :: Addr -> GMHeap -> GMHeap
markFrom addr heap = case hLoad heap addr of
    node@(NNum _) -> hUpdate heap addr $ NMarked node
    node@(NApp a1 a2) ->
        let heap'  = hUpdate heap addr $ NMarked node
            heap'' = markFrom a1 heap'
        in markFrom a2 heap''
    node@(NGlobal _ _) -> hUpdate heap addr $ NMarked node
    node@(NPointer a) ->
        let heap' = hUpdate heap addr $ NMarked node
        in markFrom a heap'
    node@(NConstructor _ addrs) ->
        let heap' = hUpdate heap addr $ NMarked node
        in foldr markFrom heap' addrs
    _ -> heap

-- Scan all nodes freeing unmarked nodes and unmarking marked nodes
scanHeap :: GMHeap -> GMHeap
scanHeap heap = foldr scanFrom heap addresses where
    addresses = hAddresses heap
    scanFrom addr heap = case hLoad heap addr of
        NMarked node -> hUpdate heap addr node
        _            -> hFree heap addr


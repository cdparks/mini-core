module MiniCore.GMachine (
    execute,
    isFinal
) where

import MiniCore.Types
import MiniCore.Format
import MiniCore.Heap

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import System.IO

-- Evaluation monad transformer
type Eval a = StateT GMState Stage a

-- Execute computation for its effect on GMState
type Transition = Eval ()

-- Execute computation and yield resulting GMState
type Inspect = Eval GMState

-- Public interface to GMachine takes an initial state and yield
-- a final state
execute :: Bool -> Bool -> GMState -> Stage GMState
execute loud interactive state = execStateT (evaluate loud interactive) state

-- Ask if user wants to continue in interactive mode
untilNext :: Eval ()
untilNext =
    do liftIO $ putStr "Next/Quit? [enter/q]: "
       liftIO $ hFlush stdout
       line <- liftIO getLine
       case line of
           "q"   -> throwError "Halting..."
           ""    -> return ()
           _     -> untilNext

-- Move from state to state until final state is reached
evaluate :: Bool -> Bool -> Inspect
evaluate loud interactive =
    do state <- get
       when (loud || interactive) $
           lift $ trace $ formatState state
       when interactive $
           untilNext
       if isFinal state then
           return state
       else
           step >> doAdmin >> evaluate loud interactive

-- Update machine statistics. Collect garbage if heap has grown
-- too large
doAdmin :: Transition
doAdmin = incSteps {-
    do if hTooLarge $ gmHeap state then
           incSteps >> incCollections -- >> gc
       else
           incSteps -}

-- Increment number of steps
incSteps :: Transition
incSteps =
    do stats <- gets gmStats
       let stats' = stats { gmSteps = gmSteps stats + 1 }
       modify $ \s -> s { gmStats = stats' }

-- Increment number of collections
incCollections :: Transition
incCollections =
    do stats <- gets gmStats
       let stats' = stats { gmCollections = gmCollections stats + 1 }
       modify $ \s -> s { gmStats = stats' }

-- Finished when no more code to execute
isFinal :: GMState -> Bool
isFinal = null . gmCode

-- Transition to next state
step :: Transition
step =
    do (c:code) <- gets gmCode
       modify $ \s -> s { gmCode = code }
       dispatch c

-- Push an address on the stack
pushStack :: Addr -> Transition
pushStack addr = modify $ \s -> s { gmStack = addr:gmStack s }

-- Push unboxed value on the V-stack
pushVStack :: Int -> Transition
pushVStack n = modify $ \s -> s { gmVStack = n:gmVStack s }

-- Pop address off stack
popStack :: Eval Addr
popStack =
    do (addr:stack) <- gets gmStack
       modify $ \s -> s { gmStack = stack }
       return addr

-- Pop unboxed value off stack
popVStack :: Eval Int
popVStack =
    do (n:vstack) <- gets gmVStack
       modify $ \s -> s { gmVStack = vstack }
       return n

-- Allocate a node on the heap
gmAlloc :: Node -> Eval Addr
gmAlloc node =
    do heap <- gets gmHeap
       let (heap', addr) = hAlloc heap node
       modify $ \s -> s { gmHeap = heap' }
       return addr

-- Update a node in the heap
gmUpdate :: Addr -> Node -> Transition
gmUpdate addr node =
    do heap <- gets gmHeap
       let heap' = hUpdate heap addr node
       modify $ \s -> s { gmHeap = heap' }

-- Load node from heap
gmLoad :: Addr -> Eval Node
gmLoad addr =
    do heap <- gets gmHeap
       let node = hLoad heap addr
       return node


-- Dispatch from instruction to implementation
dispatch :: Instruction -> Transition
dispatch (Pushglobal f)   = pushglobal f
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
dispatch Get              = get'
dispatch Eval             = eval
dispatch Unwind           = unwind
dispatch Print            = print'
dispatch LParen           = lparen
dispatch RParen           = rparen
dispatch Space            = space
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

-- Find global node by name
-- (o, Pushglobal f : i, s,     d, v, h, m[(f, a)])
-- (o, i,                a : s, d, v, h, m)
pushglobal :: Name -> Transition
pushglobal f =
    do globals <- gets gmGlobals
       let addr = fromJust $ lookup f globals
       pushStack addr

-- Allocate number in heap and push on stack
-- (o, Pushint n : i, s,     d, v, h,              m)
-- (o, i,             a : s, d, v, h[(a, NNum n)], m)
pushint :: Int -> Transition
pushint n =
    do addr <- gmAlloc $ NNum n
       pushStack addr

-- Push unboxed integer onto V-stack
-- (o, Pushbasic n : i, s, d, v,     h, m)
-- (o, i,               s, d, n : v, h, m)
pushbasic :: Int -> Transition
pushbasic = pushVStack

-- Push address of argument on stack
-- (o, Push n : i, a0 : ... : an+1 : s,         d, v, h[(an + 1, NApp an an')], m)
-- (o, i,          an' : a0 : ... : an + 1 : s, d, v, h,                        m)
push :: Int -> Transition
push n =
    do stack <- gets gmStack
       let addr = stack !! n
       pushStack addr

-- Pop n items from the stack
-- (o, Pop n : i, a1 : ... : an : s, d, v, h, m)
-- (o, i,         s,                 d, v, h, m)
pop :: Int -> Transition
pop n =
    do stack <- gets gmStack
       modify $ \s -> s { gmStack = drop n stack }

-- Remove items from stack leaving top-of-stack
-- (o, Slide n : i, a0 : ... : an : s, d, v, h, m)
-- (o, i,           a0 : s,            d, v, h, m)
slide :: Int -> Transition
slide n =
    do (top:stack) <- gets gmStack
       modify $ \s -> s { gmStack = top:drop n stack }

-- Allocate n nodes in the heap and put their addresses on the stack
-- (o, Alloc n : i, s,                 d, v, h,                                                  m)
-- (o, i,           a1 : ... : an : s, d, v, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
alloc :: Int -> Transition
alloc n =
    when (n > 0) $
      do addr <- gmAlloc $ NPointer hNull
         pushStack addr
         alloc $ pred n

-- Replace root of redex with pointer to top-of-stack
-- (o, Update n : i, a : a0 : ... : an : s, d, v, h,                  m)
-- (o, i,            a0 : ... : an : s,     d, v, h[(an, NPointer a), m)
update :: Int -> Transition
update n =
    do (addr:stack) <- gets gmStack
       gmUpdate (stack !! n) $ NPointer addr
       modify $ \s -> s { gmStack = stack }

-- Build constructor node in heap from stack elements
-- (o, Pack t n : i, a1 : ... : an : s, d, v, h,                                    m)
-- (o, i,            a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
pack :: Int -> Int -> Transition
pack tag arity =
    do stack <- gets gmStack
       when (length stack < arity) $ 
           throwError "Not enough arguments to saturate constructor"
       let (args, stack') = splitAt arity stack
       addr <- gmAlloc $ NConstructor tag args
       modify $ \s -> s { gmStack = addr:stack' }

-- Evaluate top-of-stack to WHNF and use tag to jump to code
-- (o, Casejump [..., t -> i', ...] : i, a : s, d, v, h[(a, NConstructor t cs)], m)
-- (o, i' ++ i,                          a : s, d, v, h,                         m)
casejump :: [(Int, GMCode)] -> Transition
casejump alts =
    do (addr:_) <- gets gmStack
       (NConstructor tag _) <- gmLoad addr
       cons <- gets gmCons
       branch <- case lookup tag alts of
                   Just branch -> return branch
                   Nothing     -> throwError $ "No case for constructor " ++ cons !! tag
       modify $ \s -> s { gmCode = branch ++ gmCode s }

-- Simple branch using top-of-V-stack
-- If top-of-V-stack is 2 (True tag):
-- (o, Cond t f : i, s, d, 2 : v, h, m)
-- (o, t ++ i,       s, d, v,     h, m)
-- If top-of-V-stack is 1 (False tag):
-- (o, Cond t f : i, s, d, 1 : v, h, m)
-- (o, f ++ i,       s, d, v,     h, m)
cond :: GMCode -> GMCode -> Transition
cond consequent alternative =
    do (condition:vstack) <- gets gmVStack
       branch <- case condition of
                  1 -> return alternative
                  2 -> return consequent
                  _ -> throwError $ "Non-Boolean " ++ show condition ++ " used in Boolean context"
       modify $ \s -> s { gmCode = branch ++ gmCode s, gmVStack = vstack }

-- Destructure constructor onto stack
-- (o, Split n : i, a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i,           a1 : ... : an : s, d, v, h,                                    m)
split :: Int -> Transition
split n =
    do (addr:stack) <- gets gmStack
       (NConstructor _ args) <- gmLoad addr
       when (length args /= n) $
           throwError $ "Cannot destructure constructor into " ++ show n ++ " components"
       modify $ \s -> s { gmStack = args ++ stack }

-- Print numbers and constructor components by adding values to output list
-- (o,      Print : i, a : s, d, v, h[(a, NNum n)], m)
-- (o ++ n, i,         s,     d, v, h,              m)
-- Or
-- (o, Print : i, a : s,                 d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i' ++ i,   a1 : ... : an : s,     d, v, h,                                    m)
-- where i' = concat (take n (repeat [Eval, Print]))
print' :: Transition
print' =
    do addr <- popStack
       node <- gmLoad addr
       doPrint node
  where
    doPrint (NNum n) =
        modify $ \s -> s { gmOutput = show n:gmOutput s }

    doPrint (NConstructor tag args) =
        do cons <- gets gmCons
           if length args > 0 then
               modify $ \s -> s
                   { gmOutput = (cons !! tag):"(":gmOutput s
                   , gmCode = printN (length args) ++ [RParen] ++ gmCode s
                   , gmStack = args ++ gmStack s
                   }
           else
               modify $ \s -> s { gmOutput = (cons !! tag):gmOutput s }

    doPrint (NGlobal _ _) =
        modify $ \s -> s { gmOutput = "<function>":gmOutput s }

    doPrint (NApp _ _) =
        modify $ \s -> s { gmOutput = "<function>":gmOutput s }

    doPrint x = throwError $ "tried to print " ++ show x

    printN n = concat $ take n $ repeat [Space, Eval, Print]

-- Add punctuation to output for constructor applications
lparen = modify $ \s -> s { gmOutput = "(":gmOutput s }
rparen = modify $ \s -> s { gmOutput = ")":gmOutput s }
space  = modify $ \s -> s { gmOutput = " ":gmOutput s }

-- Build application from 2 addresses on top of stack
-- (o, Mkap : i, a1 : a2 : s, d, v, h,                  m)
-- (o, i,        a : s,       d, v, h[(a, NApp a1 a2)], m)
mkap :: Transition
mkap =
    do a1 <- popStack
       a2 <- popStack
       addr <- gmAlloc $ NApp a1 a2
       pushStack addr

-- Box top-of-V-stack into heap as integer, put address on top-of-stack
-- (o, Mkint : i, s,     d, n : v, h,              m)
-- (o, i,         a : s, d, v,     h[(a, NNum n)], m)
mkint :: Transition
mkint =
    do n <- popVStack
       addr <- gmAlloc $ NNum n
       pushStack addr

-- Box top-of-V-stack into heap as Boolean, put address on top-of-stack
-- (o, Mkbool : i, s,     d, b : v, h,                         m)
-- (o, i,          a : s, d, v,     h[(a, NConstructor b [])], m)
mkbool :: Transition
mkbool =
    do b <- popVStack
       addr <- gmAlloc $ NConstructor b []
       pushStack addr

-- Unbox top-of-stack and put on V-stack
-- (o, Get : i, a : s, d, v,     h[(a, NNum n)], m)
-- (o, i,       s,     d, n : v, h,              m)
-- or
-- (o, Get : i, a : s, d, v,     h[(a, NConstructor b [])], m)
-- (o, i,       s,     d, b : v, h,                         m)
get' :: Transition
get' =
    do addr <- popStack
       node <- gmLoad addr
       unboxed <- case node of
                    (NNum n)            -> return n
                    (NConstructor b []) -> return b
                    _                   -> throwError $ "Cannot put node " ++ show node ++ " on V-stack"
       pushVStack unboxed

-- Put bottom of stack, V-stack, and instructions on dump.
-- Leave top-of-stack as only element on stack and unwind.
-- (o, Eval : i, a : s, d,            v,  h, m)
-- (o, [Unwind], [a],   (i, s, v : d, [], h, m)
eval :: Transition
eval =
    do (addr:stack) <- gets gmStack
       vstack <- gets gmVStack
       dump <- gets gmDump
       code <- gets gmCode
       modify $ \s -> s
            { gmCode   = [Unwind]
            , gmStack  = [addr]
            , gmVStack = []
            , gmDump   = (code, stack, vstack):dump
            }

-- If dump is not empty, restore to machine state. Otherwise,
-- halt execution.
restoreDump :: Addr -> Transition
restoreDump addr =
    do dump <- gets gmDump
       case dump of
         (code, stack, vstack):dump ->
            modify $ \s -> s
                { gmCode   = code
                , gmStack  = addr:stack
                , gmVStack = vstack
                , gmDump = dump
                }
         _ -> modify $ \s -> s { gmCode = [] }

-- Use top of stack to build next state
unwind :: Transition
unwind =
    do (addr:_) <- gets gmStack
       node <- gmLoad addr
       newState node
  where
    -- Number on stack and empty dump; G-Machine is terminating.
    -- (o, [Unwind], a : s, [], v, h[(a, NNum n)], m)
    -- (o, [],       a : s, [], v, h,            m)
    -- Or number on stack and not-empty dump; restore code and stack
    -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, NNum n)], m)
    -- (o, c,        a : s', d,               v', h,              m)
    newState (NNum _) =
        do addr <- popStack
           restoreDump addr

    -- Constructor on stack and empty dump; G-Machine is terminating.
    -- (o, [Unwind], a : s, [], v, h[(a, Constructor tar args)], m)
    -- (o, [],       a : s, [], v, h,            m)
    -- Or Constructor on stack and not-empty dump; restore code and stack
    -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, Constructor tag args)], m)
    -- (o, c,        a : s', d,               v', h,                            m)
    newState (NConstructor _ _) =
        do addr <- popStack
           restoreDump addr

    -- Application; keep unwinding applications onto stack
    -- (o, [Unwind], a : s,      d, v, h[(a, NApp a1 a2)], m)
    -- (o, [Unwind], a1 : a : s, d, v, h,                  m)
    newState (NApp a1 a2) =
        do (addr:stack) <- gets gmStack
           modify $ \s -> s
               { gmCode  = [Unwind]
               , gmStack = a1:addr:stack
               }

    -- Pointer; dereference and replace top-of-stack
    -- (o, [Unwind], a0 : s, d, v, h[(a0, NPointer a)], m)
    -- (o, [Unwind], a : s,  d, v, h,                   m)
    newState (NPointer a) =
        do (_:stack) <- gets gmStack
           modify $ \s -> s
               { gmCode  = [Unwind]
               , gmStack = a:stack
               }

    -- Global; put code for global in code component of machine.
    -- (o, [Unwind], a0 : ... : an : s,   d, v, h[(a0, NGlobal n c), (NApp a0 a1'), ..., (NApp an-1, an')], m)
    -- (o, c,        a1' : ... : an' : s, d, v, h,                                                          m)
    -- If we're evaluating something to WHNF, there will be information on the
    -- dump. In this case, we don't need to fully apply the combinator, and if we
    -- can't, we should just return the root of the redex:
    -- (o, [Unwind], [a0, ..., ak], (i, s) : d, v, h[(a0, NGlobal n c)], m)
    -- (o, i,        ak : s,                 d, v, h,                    m) when k < n
    newState (NGlobal n code) =
        do (addr:stack) <- gets gmStack
           if length stack < n then
               restoreDump $ last $ addr:stack
           else
               do stack <- rearrange n
                  modify $ \s -> s
                      { gmCode  = code
                      , gmStack = stack
                      }

-- Pull n arguments directly onto the stack out of NApp nodes
rearrange :: Int -> Eval GMStack
rearrange n =
    do stack <- gets gmStack
       args <- mapM getArg $ tail stack
       return $ take n args ++ drop n stack

-- Get argument component from application
getArg :: Addr -> Eval Addr
getArg addr =
    do node <- gmLoad addr
       case node of
         NApp _ arg -> return arg
         _          -> throwError "Attempted to load argument to non-application node"

-- Generate a state transition from a unary arithmetic function
arithUnary :: (Int -> Int) -> Transition
arithUnary op =
    do x <- popVStack
       pushVStack $ op x

-- Generate a state transition from a binary arithmetic function
arithBinary :: (Int -> Int -> Int) -> Transition
arithBinary op =
    do x <- popVStack
       y <- popVStack
       pushVStack $ op x y

-- Generate a state transition from a binary comparison function
compBinary :: (Int -> Int -> Bool) -> Transition
compBinary op =
    do x <- popVStack
       y <- popVStack
       case op x y of
         True  -> pushVStack 2 -- True tag
         False -> pushVStack 1 -- False tag

{- 
-- Garbage collection state is a function of
-- the current heap and (usually) a machine component
type GCState a = State GMHeap a

-- Simple mark and scan garbage collection
gc :: GMState -> GMState
gc state = state' { gmHeap = heap' } where
    state' = evalState (mark state) $ gmHeap state
    heap'  = hIncreaseMax $ scanHeap $ gmHeap state'

-- Walk machine state marking roots and removing indirections
mark :: GMState -> GCState GMState
mark state =
    do dump    <- markFromDump    $ gmDump state
       stack   <- markFromStack   $ gmStack state
       globals <- markFromGlobals $ gmGlobals state
       heap    <- get
       return state
         { gmHeap = heap
         , gmDump = dump
         , gmStack = stack
         , gmGlobals = globals
         }

-- Mark all root addresses in dump's stack component
markFromDump :: GMDump -> GCState GMDump
markFromDump = mapM $ \(code, stack, vstack) ->
    do stack' <- markFromStack stack
       return (code, stack', vstack)

-- Mark all root addresses in stack
markFromStack :: GMStack -> GCState GMStack
markFromStack = mapM markFrom

-- Mark all root addresses in globals
markFromGlobals :: GMGlobals -> GCState GMGlobals
markFromGlobals = mapM $ \(name, addr) ->
    do addr' <- markFrom addr
       return (name, addr')

-- Start from address and mark all reachable addresses from it. Replace any
-- pointer nodes by what they point to.
markFrom :: Addr -> GCState Addr
markFrom addr =
    do heap <- get
       case hLoad heap addr of
         node@(NNum _) ->
            do modify $ \s -> hUpdate s addr $ NMarked node
               return addr
         node@(NApp a1 a2) ->
            -- Visit this node to avoid looping
            do modify $ \s -> hUpdate s addr $ NMarked node
               a1' <- markFrom a1
               a2' <- markFrom a2
               -- Update addresses that may have changed
               modify $ \s -> hUpdate s addr $ NMarked $ NApp a1' a2'
               return addr
          node@(NGlobal _ _) ->
            do modify $ \s -> hUpdate heap addr $ NMarked node
               return addr
          node@(NPointer a)
            | isNullAddr a ->
                do modify $ \s -> hUpdate heap addr $ NMarked node
                   return addr
            | otherwise    -> markFrom a
          node@(NConstructor tag addrs) ->
            -- Visit this node to avoid looping
            do modify $ \s -> hUpdate s addr $ NMarked node
               addrs' <- mapM markFrom addrs
               -- Update addresses that may have changed
               modify $ \s -> hUpdate s addr $ NMarked $ NConstructor tag addrs'
               return addr
        node -> return addr

-- Scan all nodes freeing unmarked nodes and unmarking marked nodes
scanHeap :: GMHeap -> GMHeap
scanHeap heap = foldr scanFrom heap addresses
  where
    addresses = hAddresses heap
    scanFrom addr heap = case hLoad heap addr of
        NMarked node -> hUpdate heap addr node
        _            -> hFree heap addr
-}


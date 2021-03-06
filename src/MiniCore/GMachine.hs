{-# LANGUAGE FlexibleContexts #-}

module MiniCore.GMachine
  ( execute
  , isFinal
  ) where

import MiniCore.Types
import MiniCore.Format
import MiniCore.Heap

import Data.List
import Data.Maybe
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Control.Monad
import Control.Applicative

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
untilNext = do
  liftIO (putStr "Next/Quit? [enter/q]: ")
  liftIO (hFlush stdout)
  line <- liftIO getLine
  case line of
    "q" -> runtimeError "Halting"
    ""  -> return ()
    _   -> untilNext

-- Move from state to state until final state is reached
evaluate :: Bool -> Bool -> Inspect
evaluate loud interactive = do
  state <- get
  when (loud || interactive) $
    lift (trace (formatState state))
  when interactive $
    untilNext
  if isFinal state
    then return state
    else do
      step
      doAdmin
      evaluate loud interactive

-- Update machine statistics. Collect garbage if heap has grown
-- too large
doAdmin :: Transition
doAdmin = do
  incSteps
  heap <- gets gmHeap
  when (hTooLarge heap) $ do
    incCollections
    gc

-- Increment number of steps
incSteps :: Transition
incSteps = do
  stats <- gets gmStats
  let stats' = stats { gmSteps = gmSteps stats + 1 }
  modify (\s -> s { gmStats = stats' })

-- Increment number of collections
incCollections :: Transition
incCollections = do
  stats <- gets gmStats
  let stats' = stats { gmCollections = gmCollections stats + 1 }
  modify (\s -> s { gmStats = stats' })

-- Finished when no more code to execute
isFinal :: GMState -> Bool
isFinal = null . gmCode

-- Transition to next state
step :: Transition
step = do
  c:code <- gets gmCode
  modify (\s -> s { gmCode = code })
  dispatch c

-- Push an address on the stack
pushStack :: Addr -> Transition
pushStack addr = modify (\s -> s { gmStack = addr:gmStack s })

-- Push unboxed value on the V-stack
pushVStack :: Int -> Transition
pushVStack n = modify (\s -> s { gmVStack = n:gmVStack s })

-- Pop address off stack
popStack :: Eval Addr
popStack = do
  addr:stack <- gets gmStack
  modify (\s -> s { gmStack = stack })
  return addr

-- Pop unboxed value off stack
popVStack :: Eval Int
popVStack = do
  n:vstack <- gets gmVStack
  modify (\s -> s { gmVStack = vstack })
  return n

-- Allocate a node on the heap
gmAlloc :: Node -> Eval Addr
gmAlloc node = do
  heap <- gets gmHeap
  let (heap', addr) = hAlloc heap node
  modify (\s -> s { gmHeap = heap' })
  return addr

-- Update a node in the heap
gmUpdate :: Addr -> Node -> Transition
gmUpdate addr node = do
  heap <- gets gmHeap
  let heap' = hUpdate heap addr node
  modify (\s -> s { gmHeap = heap' })

-- Load node from heap
gmLoad :: Addr -> Eval Node
gmLoad addr = do
  heap <- gets gmHeap
  let node = hLoad heap addr
  return node


-- Dispatch from instruction to implementation
dispatch :: Instruction -> Transition
dispatch i = case i of
  Pushglobal f  -> pushglobal f
  Pushint n     -> pushint n
  Pushbasic n   -> pushbasic n
  Push n        -> push n
  Pop n         -> pop n
  Slide n       -> slide n
  Alloc n       -> alloc n
  Update n      -> update n
  Pack t n      -> pack t n
  Casejump alts -> casejump alts
  Cond t f      -> cond t f
  Split n       -> split n
  Mkap          -> mkap
  Mkint         -> mkint
  Mkbool        -> mkbool
  Get           -> get'
  Eval          -> eval
  Unwind        -> unwind
  Print         -> print'
  LParen        -> lparen
  RParen        -> rparen
  Space         -> space
  Add           -> arithBinary (+)
  Sub           -> arithBinary (-)
  Mul           -> arithBinary (*)
  Div           -> arithBinary div
  Neg           -> arithUnary negate
  Eq            -> compBinary (==)
  Ne            -> compBinary (/=)
  Lt            -> compBinary (<)
  Le            -> compBinary (<=)
  Gt            -> compBinary (>)
  Ge            -> compBinary (>=)

-- Find global node by name
-- (o, Pushglobal f : i, s,     d, v, h, m[(f, a)])
-- (o, i,                a : s, d, v, h, m)
pushglobal :: Name -> Transition
pushglobal f = do
  globals <- gets gmGlobals
  let addr = fromJust (lookup f globals)
  pushStack addr

-- Allocate number in heap and push on stack
-- (o, Pushint n : i, s,     d, v, h,              m)
-- (o, i,             a : s, d, v, h[(a, NNum n)], m)
pushint :: Int -> Transition
pushint n = do
  addr <- gmAlloc (NNum n)
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
push n = do
  stack <- gets gmStack
  let addr = stack !! n
  pushStack addr

-- Pop n items from the stack
-- (o, Pop n : i, a1 : ... : an : s, d, v, h, m)
-- (o, i,         s,                 d, v, h, m)
pop :: Int -> Transition
pop n = do
  stack <- gets gmStack
  modify (\s -> s { gmStack = drop n stack })

-- Remove items from stack leaving top-of-stack
-- (o, Slide n : i, a0 : ... : an : s, d, v, h, m)
-- (o, i,           a0 : s,            d, v, h, m)
slide :: Int -> Transition
slide n = do
  top:stack <- gets gmStack
  modify (\s -> s { gmStack = top:drop n stack })

-- Allocate n nodes in the heap and put their addresses on the stack
-- (o, Alloc n : i, s,                 d, v, h,                                                  m)
-- (o, i,           a1 : ... : an : s, d, v, h[(a1, NPointer hNull), ..., (an, NPointer hNull)], m)
alloc :: Int -> Transition
alloc n =
  when (n > 0) $ do
    addr <- gmAlloc (NPointer hNull)
    pushStack addr
    alloc (pred n)

-- Replace root of redex with pointer to top-of-stack
-- (o, Update n : i, a : a0 : ... : an : s, d, v, h,                  m)
-- (o, i,            a0 : ... : an : s,     d, v, h[(an, NPointer a), m)
update :: Int -> Transition
update n = do
    addr:stack <- gets gmStack
    gmUpdate (stack !! n) (NPointer addr)
    modify (\s -> s { gmStack = stack })

-- Build constructor node in heap from stack elements
-- (o, Pack t n : i, a1 : ... : an : s, d, v, h,                                    m)
-- (o, i,            a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
pack :: Int -> Int -> Transition
pack tag arity = do
  stack <- gets gmStack
  when (length stack < arity) $
    runtimeError "Not enough arguments to saturate constructor"
  let (args, stack') = splitAt arity stack
  addr <- gmAlloc (NConstructor tag args)
  modify (\s -> s { gmStack = addr:stack' })

-- Evaluate top-of-stack to WHNF and use tag to jump to code
-- (o, Casejump [..., t -> i', ...] : i, a : s, d, v, h[(a, NConstructor t cs)], m)
-- (o, i' ++ i,                          a : s, d, v, h,                         m)
casejump :: [(Int, GMCode)] -> Transition
casejump alts = do
  addr:_ <- gets gmStack
  (NConstructor tag _) <- gmLoad addr
  cons <- gets gmCons
  case lookup tag alts of
    Just branch -> modify (\s -> s { gmCode = branch ++ gmCode s })
    Nothing     -> runtimeError ("No case for constructor " ++ cons !! tag)

-- Simple branch using top-of-V-stack
-- If top-of-V-stack is 2 (True tag):
-- (o, Cond t f : i, s, d, 2 : v, h, m)
-- (o, t ++ i,       s, d, v,     h, m)
-- If top-of-V-stack is 1 (False tag):
-- (o, Cond t f : i, s, d, 1 : v, h, m)
-- (o, f ++ i,       s, d, v,     h, m)
cond :: GMCode -> GMCode -> Transition
cond consequent alternative = do
  condition:vstack <- gets gmVStack
  branch <- case condition of
    1 -> return alternative
    2 -> return consequent
    _ -> runtimeError ("Non-Boolean " ++ show condition ++ " used in Boolean context")
  modify (\s -> s { gmCode = branch ++ gmCode s, gmVStack = vstack })

-- Destructure constructor onto stack
-- (o, Split n : i, a : s,             d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i,           a1 : ... : an : s, d, v, h,                                    m)
split :: Int -> Transition
split n = do
  addr:stack <- gets gmStack
  (NConstructor _ args) <- gmLoad addr
  when (length args /= n) $
    runtimeError ("Cannot destructure constructor into " ++ show n ++ " components")
  modify (\s -> s { gmStack = args ++ stack })

-- Print numbers and constructor components by adding values to output list
-- (o,      Print : i, a : s, d, v, h[(a, NNum n)], m)
-- (o ++ n, i,         s,     d, v, h,              m)
-- Or
-- (o, Print : i, a : s,                 d, v, h[(a, NConstructor t [a1, ..., an])], m)
-- (o, i' ++ i,   a1 : ... : an : s,     d, v, h,                                    m)
-- where i' = concat (take n (repeat [Eval, Print]))
print' :: Transition
print' = do
  addr <- popStack
  node <- gmLoad addr
  doPrint node
 where
  doPrint (NNum n) =
    modify $ \s -> s { gmOutput = show n:gmOutput s }

  doPrint (NConstructor tag args) = do
    cons <- gets gmCons
    if length args > 0
      then modify $ \s -> s
            { gmOutput = (cons !! tag):"(":gmOutput s
            , gmCode = printN (length args) ++ [RParen] ++ gmCode s
            , gmStack = args ++ gmStack s
            }
      else modify (\s -> s { gmOutput = (cons !! tag):gmOutput s })

  doPrint (NGlobal _ _) =
    modify (\s -> s { gmOutput = "<function>":gmOutput s })

  doPrint (NApp _ _) =
    modify (\s -> s { gmOutput = "<function>":gmOutput s })

  doPrint x = runtimeError ("tried to print " ++ show x)

  printN n = concat (take n (repeat [Space, Eval, Print]))

-- Add punctuation to output for constructor applications
lparen = modify (\s -> s { gmOutput = "(":gmOutput s })
rparen = modify (\s -> s { gmOutput = ")":gmOutput s })
space  = modify (\s -> s { gmOutput = " ":gmOutput s })

-- Build application from 2 addresses on top of stack
-- (o, Mkap : i, a1 : a2 : s, d, v, h,                  m)
-- (o, i,        a : s,       d, v, h[(a, NApp a1 a2)], m)
mkap :: Transition
mkap = do
  a1 <- popStack
  a2 <- popStack
  addr <- gmAlloc (NApp a1 a2)
  pushStack addr

-- Box top-of-V-stack into heap as integer, put address on top-of-stack
-- (o, Mkint : i, s,     d, n : v, h,              m)
-- (o, i,         a : s, d, v,     h[(a, NNum n)], m)
mkint :: Transition
mkint = do
  n <- popVStack
  addr <- gmAlloc (NNum n)
  pushStack addr

-- Box top-of-V-stack into heap as Boolean, put address on top-of-stack
-- (o, Mkbool : i, s,     d, b : v, h,                         m)
-- (o, i,          a : s, d, v,     h[(a, NConstructor b [])], m)
mkbool :: Transition
mkbool = do
  b <- popVStack
  addr <- gmAlloc (NConstructor b [])
  pushStack addr

-- Unbox top-of-stack and put on V-stack
-- (o, Get : i, a : s, d, v,     h[(a, NNum n)], m)
-- (o, i,       s,     d, n : v, h,              m)
-- or
-- (o, Get : i, a : s, d, v,     h[(a, NConstructor b [])], m)
-- (o, i,       s,     d, b : v, h,                         m)
get' :: Transition
get' = do
  addr <- popStack
  node <- gmLoad addr
  unboxed <- case node of
    NNum n            -> return n
    NConstructor b [] -> return b
    _                  -> runtimeError ("Cannot put node " ++ show node ++ " on V-stack")
  pushVStack unboxed

-- Put bottom of stack, V-stack, and instructions on dump.
-- Leave top-of-stack as only element on stack and unwind.
-- (o, Eval : i, a : s, d,            v,  h, m)
-- (o, [Unwind], [a],   (i, s, v : d, [], h, m)
eval :: Transition
eval = do
  addr:stack <- gets gmStack
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
restoreDump addr = do
  dump <- gets gmDump
  case dump of
    (code, stack, vstack):dump -> modify $ \s -> s
      { gmCode   = code
      , gmStack  = addr:stack
      , gmVStack = vstack
      , gmDump = dump
      }
    _ -> modify $ \s -> s { gmCode = [] }

-- Use top of stack to build next state
unwind :: Transition
unwind = do
  addr:_ <- gets gmStack
  node <- gmLoad addr
  newState node
 where
  -- Number on stack and empty dump; G-Machine is terminating.
  -- (o, [Unwind], a : s, [], v, h[(a, NNum n)], m)
  -- (o, [],       a : s, [], v, h,            m)
  -- Or number on stack and not-empty dump; restore code and stack
  -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, NNum n)], m)
  -- (o, c,        a : s', d,               v', h,              m)
  newState (NNum _) = popStack >>= restoreDump

  -- Constructor on stack and empty dump; G-Machine is terminating.
  -- (o, [Unwind], a : s, [], v, h[(a, Constructor tar args)], m)
  -- (o, [],       a : s, [], v, h,            m)
  -- Or Constructor on stack and not-empty dump; restore code and stack
  -- (o, [Unwind], a : s,  (c, s', v') : d, v,  h[(a, Constructor tag args)], m)
  -- (o, c,        a : s', d,               v', h,                            m)
  newState (NConstructor _ _) = popStack >>= restoreDump

  -- Application; keep unwinding applications onto stack
  -- (o, [Unwind], a : s,      d, v, h[(a, NApp a1 a2)], m)
  -- (o, [Unwind], a1 : a : s, d, v, h,                  m)
  newState (NApp a1 a2) = do
    addr:stack <- gets gmStack
    modify $ \s -> s
      { gmCode  = [Unwind]
      , gmStack = a1:addr:stack
      }

  -- Pointer; dereference and replace top-of-stack
  -- (o, [Unwind], a0 : s, d, v, h[(a0, NPointer a)], m)
  -- (o, [Unwind], a : s,  d, v, h,                   m)
  newState (NPointer a) = do
    _:stack <- gets gmStack
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
  newState (NGlobal n code) = do
    addr:stack <- gets gmStack
    if length stack < n
      then restoreDump (last (addr:stack))
      else do
        stack <- rearrange n
        modify $ \s -> s
          { gmCode  = code
          , gmStack = stack
          }

-- Pull n arguments directly onto the stack out of NApp nodes
rearrange :: Int -> Eval GMStack
rearrange n = do
  stack <- gets gmStack
  args <- mapM getArg (tail stack)
  return (take n args ++ drop n stack)

-- Get argument component from application
getArg :: Addr -> Eval Addr
getArg addr = do
  node <- gmLoad addr
  case node of
      NApp _ arg -> return arg
      _          -> runtimeError ("Attempted to load argument to non-application node")

-- Generate a state transition from a unary arithmetic function
arithUnary :: (Int -> Int) -> Transition
arithUnary op = popVStack >>= pushVStack . op

-- Generate a state transition from a binary arithmetic function
arithBinary :: (Int -> Int -> Int) -> Transition
arithBinary op = do
  x <- popVStack
  y <- popVStack
  pushVStack (op x y)

-- Generate a state transition from a binary comparison function
compBinary :: (Int -> Int -> Bool) -> Transition
compBinary op = do
  x <- popVStack
  y <- popVStack
  case op x y of
    True  -> pushVStack 2 -- True tag
    False -> pushVStack 1 -- False tag

-- Simple mark and scan garbage collection
gc :: Transition
gc = do
  markFromDump
  markFromStack
  markFromGlobals
  scanHeap

-- Mark all root addresses in dump's stack component
markFromDump :: Transition
markFromDump = do
  dump <- gets gmDump >>= mapM mark
  modify (\s -> s { gmDump = dump })
 where
  mark (code, stack, vstack) = do
    stack' <- mapM markFrom stack
    return (code, stack', vstack)

-- Mark all root addresses in stack
markFromStack :: Transition
markFromStack = do
  stack <- gets gmStack >>= mapM markFrom
  modify (\s -> s { gmStack = stack })

-- Mark all root addresses in globals
markFromGlobals :: Transition
markFromGlobals = do
  globals <- gets gmGlobals >>= mapM mark
  modify (\s -> s { gmGlobals = globals })
 where
  mark (name, addr) = do
    addr' <- markFrom addr
    return (name, addr')

writeHeap :: Addr -> Node -> Eval Addr
writeHeap addr node = do
  heap <- gets gmHeap
  modify (\s -> s { gmHeap = hUpdate heap addr node })
  return addr

-- Start from address and mark all reachable addresses from it. Replace any
-- pointer nodes by what they point to.
markFrom :: Addr -> Eval Addr
markFrom addr = do
  heap <- gets gmHeap
  case hLoad heap addr of
    node@(NNum _) ->
      writeHeap addr $ NMarked node

    node@(NApp a1 a2) -> do
      -- Visit this node to avoid looping
      _ <- writeHeap addr (NMarked node)
      a1' <- markFrom a1
      a2' <- markFrom a2
      -- Update addresses that may have changed
      writeHeap addr $ NMarked $ NApp a1' a2'

    node@(NGlobal _ _) ->
      writeHeap addr $ NMarked node

    node@(NPointer a)
        | isNullAddr a -> writeHeap addr $ NMarked node
        | otherwise    -> markFrom a

    node@(NConstructor tag addrs) -> do
        -- Visit this node to avoid looping
        _ <- writeHeap addr (NMarked node)
        addrs' <- mapM markFrom addrs
        -- Update addresses that may have changed
        writeHeap addr (NMarked (NConstructor tag addrs'))

    _ -> return addr

-- Scan all nodes freeing unmarked nodes and unmarking marked nodes
scanHeap :: Transition
scanHeap = do
  heap <- gets gmHeap
  let addresses = hAddresses heap
      heap'     = foldr scanFrom heap addresses
  modify (\s -> s { gmHeap = hIncreaseMax heap' })
 where
  scanFrom addr heap =
    case hLoad heap addr of
      NMarked node -> hUpdate heap addr node
      _            -> hFree heap addr


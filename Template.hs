module Template where

-- (size, free list, environment)
import Heap
import Expr
import Parse (parseCore)

import Data.List
import Text.PrettyPrint

-- Stack of pointers to nodes in the spine 
-- of the current expression
type Stack = [Addr]

-- Pointers to supercombinators and primitives
type Globals = [(Name, Addr)]

-- State of spine stack before evaluating the argument of 
-- a strict primitive. Dummy constructor for now.
data Dump = DummyDump
initialDump = DummyDump

-- Count number of steps taken 
type Steps = Int

data Node = NApp Addr Addr
          | NCombinator Name [Name] Expr
          | NNum Int
          | NPointer Addr
            deriving Show

-- state = (s, d, h, f)
type TIState = (Stack, Dump, Heap Node, Globals, Steps)

{-
    Transitions from (s, d, h, f) -> (s', d', h', f'):

    1. Unwind a single application node onto the stack:
          (a : s,     d, h[a : NApp a1 a2], f)
       -> (a1 : a: s, d, h[a : NApp a1 a2], f)

    2. Perform supercombinator reduction updating the root of the redex:
          (a0: a1 : ... : an : s, d, h[a0 : NCombinator [x1, ..., xn] body], f)
       -> (ar : s,                d, h'[an : NPointer ar],                   f)
       where (h', ar) = instantiate h f[x -> a1, ..., xn -> an] body

    3. Handle indirection on the stack
          (a : s,  d, h[a : NPointer a1], f)
       -> (a1 : s, d, h,                  f)
-}

-- Extra definitions to add to initial global environment
extraDefs = []

-- Generate initial state from AST
compile :: Program -> TIState
compile program = (stack, dump, heap, globals, steps) where
    defs = program ++ prelude ++ extraDefs
    (heap, globals) = buildInitialHeap defs
    stack = [mainAddr]
    steps = 0
    dump = initialDump
    mainAddr = case lookup "main" globals of
        Just addr -> addr
        Nothing   -> error "main is not defined"

-- Build initial heap from list of supercombinators
buildInitialHeap :: [Combinator] -> (Heap Node, Globals)
buildInitialHeap = mapAccumL allocCombinator heapInit where
    allocCombinator heap (name, args, body) = (heap', (name, addr)) where
        (heap', addr) = alloc heap (NCombinator name args body)

-- Increment number of steps in reduction
incSteps :: TIState -> TIState
incSteps (stack, dump, heap, globals, steps) =
    (stack, dump, heap, globals, steps + 1)

-- Transition from one state to the next keeping track of all
-- previous states
eval :: TIState -> [TIState]
eval state = state:rest where
    next                = step (incSteps state)
    rest | isFinal state = []
         | otherwise    = eval next

-- Should reduction halt?
isFinal :: TIState -> Bool
isFinal ([addr], _, heap, _, _) = isData (load heap addr)
isFinal ([], _, _, _, _)        = error "Stack underflow"
isFinal _                       = False

-- Is current Node data?
isData :: Node -> Bool
isData (NNum _) = True
isData _        = False

-- Perform a single reduction from one state to the next
step :: TIState -> TIState
step state = dispatch (load heap top) where
    (stack@(top:rest), dump, heap, globals, steps) = state

    -- Can't apply number
    dispatch (NNum n) = error "Number applied as a function"

    -- Unwind spine onto stack
    dispatch (NApp a1 a2) = (a1:stack, dump, heap, globals, steps)

    -- Dereference pointer and replace with value on stack
    dispatch (NPointer a) = (a:rest, dump, heap, globals, steps)

    -- Apply combinator
    dispatch (NCombinator name args body) = (stack', dump, heap'', globals, steps) where
        -- Bind arguments
        env = bindings ++ globals
        bindings = zip args (getArgs heap stack)

        -- Instantiate combinator body
        (heap', result) = instantiate heap env body

        -- Update stack
        expect = length args + 1
        stack' | expect > length stack = error ("Not enough arguments for supercombinator " ++ name)
               | otherwise             = result:drop expect stack

        -- Update root of redex to point to result
        root = stack !! (expect - 1)
        heap'' = update heap' root $ NPointer result

-- Load arguments from heap
getArgs :: Heap Node -> Stack -> [Addr]
getArgs heap (combinator:stack) = map getArg stack where
    getArg addr = case load heap addr of
        (NApp fun arg) -> arg
        _              -> error "Missing argument"

-- Create heap node from expression
instantiate :: Heap Node -> [(Name, Addr)] -> Expr -> (Heap Node, Addr)
instantiate heap env expr = build expr where
    -- Build number on heap
    build (Num n) = alloc heap (NNum n)

    -- Look up variable in environment
    build (Var v) = case lookup v env of
        Just value -> (heap, value)
        Nothing    -> error ("Undefined name " ++ v)

    -- Instantiate function and argument and build application
    build (App e1 e2) =
        let (heap',  a1) = instantiate heap  env e1
            (heap'', a2) = instantiate heap' env e2
        in alloc heap'' (NApp a1 a2)

    -- Instantiate each expression, add each binding to environment, and then
    -- instantiate body
    build (Let recursive bindings body) =
        let (heap', letEnv') = addBindings bindings heap letEnv
            letEnv | recursive = env' -- letrec, bindings can refer to each other
                   | otherwise = env  -- let, bindings can refer to current environment
            env' = letEnv' ++ env
        in instantiate heap' env' body

    -- Not supported yet
    build (Cons _ _) = error "Can't instantiate constructors yet"
    build (Case _ _) = error "Can't instantiate case expressions yet"

-- Add let bindings to new heap and environment
addBindings :: [(Name, Expr)] -> Heap Node -> [(Name, Addr)] -> (Heap Node, [(Name, Addr)])
addBindings bindings heap env = foldr addBinding (heap, []) bindings where
    addBinding (name, expr) (heap', env') =
        let (heap'', addr) = instantiate heap' env expr
        in (heap'', (name, addr):env')

-- Parse, compile, and reduce program.
run :: Bool -> String -> String
run verbose = show . result . eval . compile . parseCore where
    result | verbose   = formatStates
           | otherwise = formatLast

-- Format final result of computation
formatLast :: [TIState] -> Doc
formatLast states = formatNode node where
    (stack, _, heap, _, _) = last states
    node = load heap $ head stack

-- Format all computation states
formatStates :: [TIState] -> Doc
formatStates = vcat . map formatState . zip [1..]

-- Format a single computation state 
formatState :: (Int, TIState) -> Doc
formatState (num, (stack, _, heap, _, _)) = text "State" <+> int num <> colon $$ nest 4 (formatStack heap stack)

-- Format the stack as a tree of applications
formatStack :: Heap Node -> Stack -> Doc
formatStack heap (x:xs) = foldr draw (formatHeapNode heap x) (reverse xs) where
    draw addr doc = text "@" <> nest 1 (text "---" <+> formatValue addr $$ text "\\" $$ nest 1 doc)
    formatTop addr = formatAddr addr <+> formatNode (load heap addr)
    formatValue addr = case load heap addr of
        NApp a1 a2 -> formatHeapNode heap a2
        node       -> formatAddr addr <> colon <+> formatNode node

-- Load a value from the heap. Format its address and value.
formatHeapNode :: Heap Node -> Addr -> Doc
formatHeapNode heap addr = formatAddr addr <> colon <+> formatNode (load heap addr)

-- Format a heap node
formatNode :: Node -> Doc
formatNode (NApp a1 a2) = text "NApp" <+> formatAddr a1 <+> formatAddr a2
formatNode (NCombinator name args body) = text "NCombinator" <+> text name
formatNode (NNum n) =  text "NNum" <+> int n
formatNode (NPointer a) =  text "NPointer" <+> formatAddr a

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


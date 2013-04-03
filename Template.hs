module Template where

-- (size, free list, environment)
import Heap
import Expr
import Parse (parseCore)

import Data.List
import Text.PrettyPrint
import Debug.Trace

-- List of Nodes to print once execution halts
type Output = [Node]

-- Stack of pointers to nodes in the spine 
-- of the current expression
type Stack = [Addr]

-- Pointers to supercombinators and primitives
type Globals = [(Name, Addr)]

-- Stack of stacks
type Dump = [Stack]
initialDump = []

-- Count number of steps taken 
type Steps = Int

-- Heap data
data Node = NApp Addr Addr                  -- Application
          | NCombinator Name [Name] Expr    -- Supercombinator
          | NNum Int                        -- Number
          | NPointer Addr                   -- Point to another node
          | NPrim Name Primitive            -- Primitive operation
          | NData Int [Addr]                -- Tag, components
            deriving Show

-- Primitive (strict) operations
data Primitive = Negate
               | Add
               | Subtract
               | Multiply
               | Divide
               | If
               | Greater
               | GreaterEq
               | Lesser
               | LesserEq
               | Eq
               | NotEq
               | Construct Int Int
               | CasePair
               | CaseList
               | Abort
               | Halt
               | Print
                 deriving Show

-- Count node types in heap
data Usage = Usage {
    numCount :: Int,
    appCount :: Int,
    dataCount :: Int,
    pointerCount :: Int,
    primitiveCount :: Int,
    combinatorCount :: Int
}

-- state = (o, s, d, h, f)
type TIState = (Output, Stack, Dump, Heap Node, Globals, Steps)

{-
    Transitions from (s, d, h, f) -> (s', d', h', f'):

    0. Dereference pointer arguments before unwinding
          (a : s, d, h[(a, NApp a1 a2), (a2, NPointer a3)], f)
       -> (a : s, d, h[(a, NApp a1 a3)],                    f)

    1. Unwind a single application node onto the stack:
          (a : s,     d, h[(a, NApp a1 a2)], f)
       -> (a1 : a: s, d, h,                  f)

    2. Perform supercombinator reduction updating the root of the redex:
          (a0: a1 : ... : an : s, d, h [(a0, NCombinator [x1, ..., xn] body)], f)
       -> (ar : s,                d, h'[(an, NPointer ar)],                    f)
       where (h', ar) = instantiate h f[x -> a1, ..., xn -> an] body

    3. Handle indirection on the stack
          (a : s,  d, h[(a, NPointer a1)], f)
       -> (a1 : s, d, h,                   f)

    4. Handle structured data
          (a : a1 : ... : an : [],  d, h[(a, NPrim (Construct t n)), (a1, NApp a b1) ... (an: NApp an-1, bn)], f)
       -> (an : [],                 d, h[(an, NData t [b1..bn])],                                              f)

    5. Handle unary arithmetic

       Already evaluated numeric argument
          (a : a1 : [], d, h[(a, NPrim Negate) : (a1, NApp a b) : (b, NNum n)], f)
       -> (a1 : [],     d, h[(a1, NNum (-n))],                                  f)

       Save stack above un-evaluated argument in dump and...
          (a : a1 : [], d,             h[(a, NPrim Negate) : (a1, NApp a b)], f)
       -> (b : [],      (a1 : []) : d, h,                                     f)

       ...Evaluate argument and restore stack
          (a : [], s : d, h[(a, NNum n)], f)
       -> (s,      d,     h,              f)

    6. Handle binary arithmetic

       Already evaluated numeric arguments
          (a : b : c : [], d, h[(a, NPrim Add), (b, NApp a d), (c, NApp b e), (d, NNum n), (e, NNum m)], f)
       -> (c : [],         d, h[(c, NNum (n + m))],                                                      f)

       For _ + App, save stack above un-evaluated argument in dump and...
          (a : b : c : [], d,            h[(a, NPrim Add), (b, NApp a d), (c, NApp b e), (d, NNum n), (e, NApp 1 2)], f)
       -> (c : [],         (b : []) : d, h,                                                                           f)

       ...Evaluate argument and restore stack
          (c : [], s : d, h[(c, NNum n)], f)
       -> (s,      d,     h,              f)

       Then, for App + Num, save stack above un-evaluated numeric argument and...
          (a : b : c : [], d,            h[(a, NPrim Add), (b, NApp a d), (c, NApp b e), (d, NNum 23), (e, NApp 1 2)], f)
       -> (b : c : [],     (a : []) : d, h,                                                                            f)

       ...Evaluate argument and restore stack
          (b : c : [], s : d, h[(b, NNum n)], f)
       -> (s,      d,     h,                  f)

    7. Handle if-expression
       If condition is True (NData 2 []), choose the first branch
          (w : x : y : z : [], d, h[(w, NPrim If), (x, NData 2 []), (y, NApp x j), (z, NApp y k)], f)
       -> (z : [],             d, h[(z, NPointer j)],                                              f)

       If condition is False (NData 2 []), choose the second branch
          (w : x : y : z : [], d, h[(w, NPrim If), (x, NData 1 []), (y, NApp x j), (z, NApp y k)], f)
       -> (z : [],             d, h[(z, NPointer k)],                                              f)

       For unevaluated condition, save stack above un-evaluated argument in dump...
          (w : x : y : z : [], d,            h[(w, NPrim If), (x, NApp w i), (y, NApp x j), (z, NApp y k)], f)
       -> (x : y : z : [],     (w : []) : d, h,                                                             f)

       ...Evaluate argument and restore stack
          (x : y : z : [], s : d, h[(x, NData _ [])], f)
       -> (s,              d,     h,                  f)

-}

-- Extra definitions to add to initial global environment
extraDefs = [
    ("and", ["x", "y"], App (App (App (Var "if") (Var "x")) (Var "y")) (Var "False")),
    ("or",  ["x", "y"], App (App (App (Var "if") (Var "x")) (Var "True")) (Var "y")),
    ("not", ["x"],      App (App (App (Var "if") (Var "x")) (Var "False")) (Var "True")),
    ("fst", ["p"],      App (App (Var "casePair") (Var "p")) (Var "K")),
    ("snd", ["p"],      App (App (Var "casePair") (Var "p")) (Var "K1")),
    ("head", ["ls"],    App (App (App (Var "caseList") (Var "ls")) (Var "abort")) (Var "K")),
    ("tail", ["ls"],    App (App (App (Var "caseList") (Var "ls")) (Var "abort")) (Var "K1"))]

-- Generate initial state from AST
compile :: Program -> TIState
compile program = (output, stack, dump, heap, globals, steps) where
    defs = program ++ prelude ++ extraDefs
    (heap, globals) = buildInitialHeap defs
    output = []
    stack = [mainAddr]
    steps = 0
    dump = initialDump
    mainAddr = case lookup "main" globals of
        Just addr -> addr
        Nothing   -> error "main is not defined"

-- Primitive data constructors
tiFalse = Construct 1 0
tiTrue  = Construct 2 0
tiCons  = Construct 3 2
tiNil   = Construct 4 0
tiPair  = Construct 5 2

-- Map var names to primitives
primitives = [
    ("negate", Negate),
    ("+", Add),
    ("-", Subtract),
    ("*", Multiply),
    ("/", Divide),
    ("<", Lesser),
    (">", Greater),
    ("<=", LesserEq),
    (">=", GreaterEq),
    ("==", Eq),
    ("/=", NotEq),
    ("if", If),
    ("casePair", CasePair),
    ("caseList", CaseList),
    ("abort", Abort),
    ("halt", Halt),
    ("print", Print),
    ("False", tiFalse),
    ("True",  tiTrue),
    ("Cons",  tiCons),
    ("Nil",   tiNil),
    ("Pair",  tiPair)]

-- Build initial heap from list of supercombinators
buildInitialHeap :: [Combinator] -> (Heap Node, Globals)
buildInitialHeap combinators = (heap'', caddrs ++ paddrs) where
    (heap',  caddrs) = mapAccumL allocCombinator heapInit combinators
    (heap'', paddrs)  = mapAccumL allocPrimitive heap' primitives

-- Allocate a single combinator
allocCombinator :: Heap Node -> Combinator -> (Heap Node, (Name, Addr))
allocCombinator heap (name, args, body) = (heap', (name, addr)) where
    (heap', addr) = alloc heap (NCombinator name args body)

-- Allocate a single primitive
allocPrimitive :: Heap Node -> (Name, Primitive) -> (Heap Node, (Name, Addr))
allocPrimitive heap (name, primitive) = (heap', (name, addr)) where
    (heap', addr) = alloc heap (NPrim name primitive)

-- Increment number of steps in reduction
incSteps :: TIState -> TIState
incSteps (output, stack, dump, heap, globals, steps) =
    (output, stack, dump, heap, globals, steps + 1)

-- Transition from one state to the next keeping track of all
-- previous states
eval :: TIState -> [TIState]
eval state = state:rest where
    next                 = step (incSteps state)
    rest | isFinal state = []
         | otherwise     = eval next

-- Should reduction halt?
isFinal :: TIState -> Bool
isFinal (_, [addr], [], heap, _, _) = isData value || isNum value where
    value = load heap addr
isFinal (_, [], _, _, _, _)         = True
isFinal _                           = False

-- Is current Node data?
isData :: Node -> Bool
isData (NData _ _) = True
isData _           = False

-- Is current Node a number?
isNum :: Node -> Bool
isNum (NNum _) = True
isNum _        = False

-- Is Node a representation of True?
isTrue (NData 2 []) = True
isTrue _            = False

-- Is Node a representation of False?
isFalse (NData 1 []) = True
isFalse _            = False

-- Is Node a Pair?
isPair (NData 5 [_, _]) = True
isPair _                = False

-- Is Node a list?
isList (NData 3 [_, _]) = True
isList (NData 4 [])     = True
isList _                = False

-- Is Node a Cons cell?
isCons (NData 3 [_, _]) = True
isCons _                = False

-- Is Node Nil?
isNil (NData 4 [_, _])  = True
isNil _                 = False

-- Apply a function to the components of a pair
pairApply heap (NData 5 [x, y]) f = (heap'', app) where
    (heap',  addr)  = alloc heap  (NApp f x)
    (heap'', addr') = alloc heap' (NApp addr y)
    app = load heap'' addr'
pairApply _ _ _                   = error "Function expects a pair"

-- If list is nil, return (heap, nil-value). Otherwise, return
-- (heap', f head tail).
listApply heap (NData 3 [x, xs]) _ f = (heap'', app) where
    (heap',  addr)  = alloc heap  (NApp f x)
    (heap'', addr') = alloc heap' (NApp addr xs)
    app = load heap'' addr'
listApply heap (NData 4 []) f _      = (heap, load heap f)
listApply _ _ _ _                    = error "Function expects a list"

-- Perform a single reduction from one state to the next
step :: TIState -> TIState
step state = dispatch (load heap top) where
    (output, stack@(top:rest), dump, heap, globals, steps) = state

    -- If number is on top, we must have deferred some
    -- primitive computation. Move it from the dump to the stack
    dispatch (NNum _) = case dump of
        d:ds -> (output, d ++ rest, ds, heap, globals, steps)
        _    -> error "Can't apply number as function"

    -- If a data node is on top, we must have deferred some
    -- primitive computation. Move it from the dump to the stack
    dispatch (NData _ _) = case dump of
        d:ds -> (output, d ++ rest, ds, heap, globals, steps)
        _    -> error "Can't apply data node as function"

    -- Unwind spine onto stack, removing indirections from the
    -- argument if present.
    dispatch (NApp a1 a2) = case load heap a2 of
        NPointer a3 -> (output, a1:stack, dump, replace heap top (NApp a1 a3), globals, steps)
        _           -> (output, a1:stack, dump, heap, globals, steps)

    -- Dereference pointer and replace with value on stack
    dispatch (NPointer a) = (output, a:rest, dump, heap, globals, steps)

    -- Apply combinator
    dispatch (NCombinator name args body) = (output, stack', dump, heap', globals, steps) where
        -- Bind arguments
        env = bindings ++ globals
        bindings = zip args (getArgs heap stack)

        -- Update root of redex to point to result
        root = stack !! (expect - 1)
        heap' = instantiateAndUpdate heap env body root

        -- Update stack
        expect = length args + 1
        stack' | expect > length stack = error ("Not enough arguments for supercombinator " ++ name)
               | otherwise             = root:drop expect stack

    -- Apply primitive
    dispatch (NPrim name primitive) = case primitive of
        -- Unary arithmetic
        Negate              -> primUnary  (fromUnary negate)    state

        -- Binary arithmetic
        Add                 -> primBinary (fromBinary (+))      state
        Subtract            -> primBinary (fromBinary (-))      state
        Multiply            -> primBinary (fromBinary (*))      state
        Divide              -> primBinary (fromBinary div)      state

        -- Binary relational
        Lesser              -> primBinary (fromRelational (<))  state
        Greater             -> primBinary (fromRelational (>))  state
        LesserEq            -> primBinary (fromRelational (<=)) state
        GreaterEq           -> primBinary (fromRelational (>=)) state
        Eq                  -> primBinary (fromRelational (==)) state
        NotEq               -> primBinary (fromRelational (/=)) state

        -- Structured data
        Construct tag arity -> primConstruct tag arity state
        CasePair            -> primCasePair state
        CaseList            -> primCaseList state

        -- If expression
        If                  -> primIf state

        -- Printing
        Print               -> primPrint state

        -- Early exit
        Halt                -> (output, [], dump, heap, globals, steps)
        Abort               -> error "Execution halted with abort"

-- Convert a unary arithmetic function into a function on nodes
fromUnary :: (Int -> Int) -> (Node -> Node)
fromUnary f (NNum x) = NNum $ f x
fromUnary _ _        = error "Expected numeric argument"

-- Convert a binary arithmetic function into a function on nodes
fromBinary :: (Int -> Int -> Int) -> (Node -> Node -> Node)
fromBinary f (NNum x) (NNum y) = NNum $ f x y
fromBinary _ _ _               = error "Expected numeric argument(s)"

-- Convert a binary relational function into a function on nodes
fromRelational :: (Int -> Int -> Bool) -> (Node -> Node -> Node)
fromRelational pred (NNum x) (NNum y)
    | pred x y  = NData 2 []
    | otherwise = NData 1 []
fromRelational _ _ _ = error "Expected numeric argument(s)"

-- Either apply unary primitive or set up evaluation of
-- argument to unary primitive
primUnary :: (Node -> Node) -> TIState -> TIState
primUnary f (output, (_:root:stack), dump, heap, globals, steps) = state' where
    addr = getArg heap root
    arg = load heap addr
    state' | isNum arg  = (output, root:stack, dump, replace heap root (f arg), globals, steps)
           | isData arg = error "Expected numeric argument to unary operator"
           | otherwise  = (output, addr:stack, [root]:dump, heap, globals, steps)
primUnary _ _ = error "Malformed unary primitive expression"

-- Either apply binary primitive or set up evaluation of
-- arguments to binary primitive
primBinary :: (Node -> Node -> Node) -> TIState -> TIState
primBinary f (output, (_:xRoot:yRoot:stack), dump, heap, globals, steps) = state' where
    (xAddr, yAddr) = (getArg heap xRoot, getArg heap yRoot)
    (x, y) = (load heap xAddr, load heap yAddr)
    state' | isNum x && isNum y        = (output, yRoot:stack, dump, replace heap yRoot (f x y), globals, steps)
           | isNum y && not (isData x) = (output, xAddr:yRoot:stack, [xRoot]:dump, heap, globals, steps)
           | isData y || isData x      = error "Expected numeric arguments to binary operator"
           | otherwise                 = (output, yAddr:stack,       [yRoot]:dump, heap, globals, steps)
primBinary _ _ = error "Malformed binary primitive expression"

-- If condition is evaluated, use it to choose the correct branch.
-- Otherwise, put application on dump and evaluate condition.
primIf (output, (_:c:x:y:stack), dump, heap, globals, steps) = state' where
    (cAddr, xAddr, yAddr) = (getArg heap c, getArg heap x, getArg heap y)
    cond = load heap cAddr
    state' | isTrue cond  = (output, y:stack, dump, replace heap y (NPointer xAddr), globals, steps)
           | isFalse cond = (output, y:stack, dump, replace heap y (NPointer yAddr), globals, steps)
           | isData cond  = error "Expected a Boolean condition for if"
           | otherwise    = (output, cAddr:x:y:stack, [c]:dump, heap, globals, steps)
primIf _ = error "Malformed if-expression"

-- If pair is evaluated, apply function to it. Otherwise, put application on
-- dump and evaluate pair.
primCasePair (output, (_:p:f:stack), dump, heap, globals, steps) = state' where
    (pAddr, fAddr) = (getArg heap p, getArg heap f)
    pair = load heap pAddr
    (heap', app) = pairApply heap pair fAddr
    state' | isPair pair = (output, f:stack, dump, replace heap' f app, globals, steps)
           | isData pair = error "Expected a pair as argument to casePair"
           | otherwise   = (output, pAddr:f:stack, [p]:dump, heap, globals, steps)
primCasePair _ = error "Malformed casePair-expression"

-- If list is evaluated, check if nil and apply appropriate function to it.
-- Otherwise, put application on dump and evaluate list.
primCaseList (output, (_:l:n:c:stack), dump, heap, globals, steps) = state' where
    (lAddr, nAddr, cAddr) = (getArg heap l, getArg heap n, getArg heap c)
    list = load heap lAddr
    (heap', app) = listApply heap list nAddr cAddr
    state' | isList list = (output, c:stack, dump, replace heap' c app, globals, steps)
           | isData list = error "Expected a list as argument to caseList"
           | otherwise   = (output, lAddr:n:c:stack, [l]:dump, heap, globals, steps)
primCaseList _ = error "Malformed caseList-expression"

-- If argument is evaluated, put it on the output stack.
-- Otherwise, put application on dump and evaluate argument.
primPrint (output, (_:v:n:stack), dump, heap, globals, steps) = state' where
    (vAddr, nAddr) = (getArg heap v, getArg heap n)
    value = load heap vAddr
    state' | isNum value  = (value:output, nAddr:stack, dump, heap, globals, steps)
           | isData value = error "Expected a numeric argument to print"
           | otherwise    = (output, vAddr:n:stack, [v]:dump, heap, globals, steps)
primPrint _ = error "Malformed print-expression"

-- Generate a new data node
primConstruct :: Int -> Int -> TIState -> TIState
primConstruct tag arity state = (output, stack', dump, heap', globals, steps) where
    (output, stack, dump, heap, globals, steps) = state
    expect = arity + 1
    root = stack !! (expect - 1)
    args = take arity $ getArgs heap stack
    heap' = replace heap root (NData tag args)
    stack' | expect > length stack = error ("Not enough arguments for constructor")
           | otherwise             = root:drop expect stack

-- Load arguments from heap
getArgs :: Heap Node -> Stack -> [Addr]
getArgs heap (combinator:stack) = map (getArg heap) stack

-- Load a single argument from heap
getArg :: Heap Node -> Addr -> Addr
getArg heap addr = case load heap addr of
    (NApp fun arg) -> arg
    _              -> error "Missing argument"

-- Create heap node from expression and replace redex root address 
-- to point to result
instantiateAndUpdate :: Heap Node -> [(Name, Addr)] -> Expr -> Addr -> Heap Node
instantiateAndUpdate heap env expr addr = build expr where
    -- Build number on heap
    build (Num n) = replace heap addr (NNum n)

    -- Look up variable in environment
    build (Var v) = case lookup v env of
        Just value -> replace heap addr (NPointer value)
        Nothing    -> error ("Undefined name " ++ v)

    -- Instantiate function and argument and build application
    build (App e1 e2) =
        let (heap',  a1) = instantiate heap  env e1
            (heap'', a2) = instantiate heap' env e2
        in replace heap'' addr (NApp a1 a2)

    -- Instantiate each expression, add each binding to environment, and then
    -- instantiate body
    build (Let recursive bindings body) =
        let (heap', letEnv') = addBindings bindings heap letEnv
            letEnv | recursive = env' -- letrec, bindings can refer to each other
                   | otherwise = env  -- let, bindings can refer to current environment
            env' = letEnv' ++ env
            (heap'', addr') = instantiate heap' env' body
        in replace heap'' addr (NPointer addr')

    -- Convert data constructor to node
    build (Cons tag arity) = replace heap addr (NPrim "Pack" (Construct tag arity))

    -- Not supported yet
    build (Case _ _) = error "Can't instantiate case expressions yet"

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

    -- Convert data constructor to node
    build (Cons tag arity) = alloc heap (NPrim "Pack" (Construct tag arity))

    -- Not supported yet
    build (Case _ _) = error "Can't instantiate case expressions yet"

-- Add let bindings to new heap and environment
addBindings :: [(Name, Expr)] -> Heap Node -> [(Name, Addr)] -> (Heap Node, [(Name, Addr)])
addBindings bindings heap env = foldr addBinding (heap, []) bindings where
    addBinding (name, expr) (heap', env') =
        let (heap'', addr) = instantiate heap' env expr
        in (heap'', (name, addr):env')

-- Parse, compile, reduce program, and print states
debug :: String -> String
debug = show . format . eval . compile . parseCore where
    format states = formatStates states $$ formatOutput states

-- Parse, compile, reduce program, and print output
run :: String -> String
run = show . formatOutput . eval . compile . parseCore

-- Format elements of output stack
formatOutput :: [TIState] -> Doc
formatOutput states = vcat $ map formatInteger $ reverse output where
    (output, _, _, _, _, _) = last states
    formatInteger (NNum n)  = int n
    formatInteger node      = error $ "Attempted to print " ++ (show $ formatNode node)

-- Format all computation states
formatStates :: [TIState] -> Doc
formatStates = vcat . map formatState . zip [1..]

-- Format a single computation state 
formatState :: (Int, TIState) -> Doc
formatState (num, (output, stack, _, heap, _, _)) = text "State" <+> int num <> colon $$ (nest 4 $ formatStack heap stack $$ formatHeap heap stack)

-- Format the stack as a tree of applications
formatStack :: Heap Node -> Stack -> Doc
formatStack heap (x:xs) = text "Stack" <> colon $$ nest 4 (foldr draw (formatHeapNode heap x) (reverse xs)) where
    draw addr doc = text "@" <> nest 1 (text "---" <+> formatValue addr $$ text "\\" $$ nest 1 doc)
    formatTop addr = formatAddr addr <+> formatNode (load heap addr)
    formatValue addr = case load heap addr of
        NApp a1 a2 -> formatHeapNode heap a2
        node       -> formatAddr addr <> colon <+> formatNode node
formatStack heap [] = text "Stack: empty"

-- Format the heap as number of allocations
formatHeap :: Heap Node -> Stack -> Doc
formatHeap (size, _, env) _ = text "Heap" <> colon $$ nest 4 (formatUsage size (calculateUsage env))

calculateUsage :: [(Addr, Node)] -> Usage
calculateUsage env = foldr count (Usage 0 0 0 0 0 0) (map snd env) where
    count (NNum _)            usage = usage {numCount        = numCount        usage + 1}
    count (NApp _ _)          usage = usage {appCount        = appCount        usage + 1}
    count (NData _ _)         usage = usage {dataCount       = dataCount       usage + 1}
    count (NPointer _)        usage = usage {pointerCount    = pointerCount    usage + 1}
    count (NPrim _ _)         usage = usage {primitiveCount  = primitiveCount  usage + 1}
    count (NCombinator _ _ _) usage = usage {combinatorCount = combinatorCount usage + 1}

formatUsage :: Int -> Usage -> Doc
formatUsage total usage = vcat [
    text "Numbers"      <> colon <+> int (numCount usage),
    text "Applications" <> colon <+> int (appCount usage),
    text "Data"         <> colon <+> int (dataCount usage),
    text "Pointers"     <> colon <+> int (pointerCount usage),
    text "Primitives"   <> colon <+> int (primitiveCount usage),
    text "Combinators"  <> colon <+> int (combinatorCount usage),
    text "Total"        <> colon <+> int total]

-- Load a value from the heap. Format its address and value.
formatHeapNode :: Heap Node -> Addr -> Doc
formatHeapNode heap addr = formatAddr addr <> colon <+> formatNode (load heap addr)

-- Format a heap node
formatNode :: Node -> Doc
formatNode (NApp a1 a2)                 = text "NApp" <+> formatAddr a1 <+> formatAddr a2
formatNode (NCombinator name args body) = text "NCombinator" <+> text name
formatNode (NNum n)                     = text "NNum" <+> int n
formatNode (NPointer a)                 = text "NPointer" <+> formatAddr a
formatNode (NPrim name prim)            = text "NPrim" <+> text name
formatNode (NData tag addrs)            = text "NData" <+> int tag <+> brackets (sep $ map formatAddr addrs)

-- Format an address
formatAddr :: Addr -> Doc
formatAddr addr = text "#" <> int addr


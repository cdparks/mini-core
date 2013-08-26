module MiniCore.GCompiler (
    compile
) where

import MiniCore.Types
import MiniCore.Heap

import Data.List
import Control.Arrow hiding ((<+>))
import Debug.Trace

-- Standard library
prelude :: Program
prelude = [ Combinator "I"  ["x"] $ Var "x"
          , Combinator "K"  ["x", "y"] $ Var "x"
          , Combinator "K1" ["x", "y"] $ Var "y"
          , Combinator "S"  ["f", "g", "x"] $
                App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))
          , Combinator "compose" ["f", "g", "x"] $
                App (Var "f") (App (Var "g") (Var "x"))
          , Combinator "twice" ["f"] $
                App (App (Var "compose") (Var "f")) (Var "f")
          ]

-- Extra definitions to add to the initial global environment
extraDefs = []

-- Uncompiled Primitives
primitives :: Program
primitives =
    [ Combinator "+"      ["x", "y"]      $ App (App (Var "+")  (Var "x")) (Var "y")
    , Combinator "-"      ["x", "y"]      $ App (App (Var "-")  (Var "x")) (Var "y")
    , Combinator "*"      ["x", "y"]      $ App (App (Var "*")  (Var "x")) (Var "y")
    , Combinator "/"      ["x", "y"]      $ App (App (Var "/")  (Var "x")) (Var "y")
    , Combinator "negate" ["x"]           $ App (Var "negate")  (Var "x")
    , Combinator "=="     ["x", "y"]      $ App (App (Var "==") (Var "x")) (Var "y")
    , Combinator "/="     ["x", "y"]      $ App (App (Var "/=") (Var "x")) (Var "y")
    , Combinator "<"      ["x", "y"]      $ App (App (Var "<")  (Var "x")) (Var "y")
    , Combinator "<="     ["x", "y"]      $ App (App (Var "<=") (Var "x")) (Var "y")
    , Combinator ">"      ["x", "y"]      $ App (App (Var ">")  (Var "x")) (Var "y")
    , Combinator ">="     ["x", "y"]      $ App (App (Var ">=") (Var "x")) (Var "y")
    , Combinator "&&"     ["x", "y"]      $ App (App (App (Var "if") (Var "x")) (Var "y")) (Var "False")
    , Combinator "||"     ["x", "y"]      $ App (App (App (Var "if") (Var "x")) (Var "True")) (Var "y")
    , Combinator "not"    ["x"]           $ App (App (App (Var "if") (Var "x")) (Var "False")) (Var "True")
    , Combinator "if"     ["c", "t", "f"] $ App (App (App (Var "if") (Var "c")) (Var "t")) (Var "f")
    , Combinator "False"  []              $ Cons 1 0
    , Combinator "True"   []              $ Cons 2 0
    ]

-- Instruction for each binary operator
binaryOpImpl :: [(Name, Instruction)]
binaryOpImpl = [ ("+",  Add)
               , ("-",  Sub)
               , ("*",  Mul)
               , ("/",  Div)
               , ("==", Eq)
               , ("/=", Ne)
               , (">=", Ge)
               , (">",  Gt)
               , ("<=", Le)
               , ("<",  Lt)
               ]

-- Boxing instruction for each binary operator
binaryOpBox :: [(Name, Instruction)]
binaryOpBox = [ ("+",  Mkint)
              , ("-",  Mkint)
              , ("*",  Mkint)
              , ("/",  Mkint)
              , ("==", Mkbool)
              , ("/=", Mkbool)
              , (">=", Mkbool)
              , (">",  Mkbool)
              , ("<=", Mkbool)
              , ("<",  Mkbool)
              ]

-- Instruction for each unary operator
unaryOpImpl :: [(Name, Instruction)]
unaryOpImpl = [("negate", Neg)]

-- Boxing instruction for each unary operator
unaryOpBox :: [(Name, Instruction)]
unaryOpBox = [("negate", Mkint)]

-- Used for a number of compilation schemes
type GMCompiler = GMEnvironment -> Expr -> GMCode

-- Turn program into initial G-Machine state
compile :: Program -> Stage GMState
compile program = return GMState
    { gmOutput  = []
    , gmCode    = codeInit
    , gmStack   = []
    , gmDump    = []
    , gmVStack  = []
    , gmHeap    = heap
    , gmGlobals = globals
    , gmStats   = statInit
    }
  where
    (heap, globals) = buildInitialHeap program

-- Push main and unwind
codeInit :: GMCode
codeInit = [Pushglobal "main", Eval, Print]

-- Start at step zero
statInit :: GMStats
statInit = GMStats 0 0

-- Instantiate supercombinators in heap
buildInitialHeap :: Program -> (GMHeap, GMGlobals)
buildInitialHeap program = mapAccumL allocSC hInit compiled where
    compiled = map compileSC $ prelude ++ extraDefs ++ program ++ primitives

-- Allocate a supercombinator and return the new heap
allocSC :: GMHeap -> (Name, Int, GMCode) -> (GMHeap, (Name, Addr))
allocSC heap (name, arity, instructions) = (heap', (name, addr)) where
    (heap', addr) = hAlloc heap (NGlobal arity instructions)

-- Compile supercombinator f with formal parameters x1...xn by
-- compiling f's body e in the environment created by substituting
-- the actual parameters for the formal parameters
-- SC(f x1 ... xn = e) = R(e) [x1 -> 0, ..., xn -> n - 1] n
compileSC :: Declaration -> (Name, Int, GMCode)
compileSC (Combinator name args body) = (name, length args, compileR (zip args [0..]) body)

-- Scheme R[e] p d generates code which instantiates the expression
-- e in environment p, for a supercombinator of arity d, and then
-- proceeds to unwind the resulting stack
compileR :: GMCompiler
compileR env (Let recursive defs body)
    | recursive = compileLetrec compileR env defs body
    | otherwise = compileLet    compileR env defs body
compileR env e@(App (App (App (Var "if") cond) t) f) =
    compileB env cond ++ [Cond (compileR env t) (compileR env f)]
compileR env (Case e alts) = compileE env e ++ [Casejump $ compileD env compileR alts]
compileR env e = compileE env e ++ [Update n, Pop n, Unwind] where
    n = length env

-- Scheme E[e] p compiles code that evaluates an expression e to
-- WHNF in environment p, leaving a pointer to the expression on
-- top of the stack.
compileE :: GMCompiler
compileE env (Num n) = [Pushint n]
compileE env (Let recursive defs body)
    | recursive = compileLetrec compileE env defs body ++ [Slide (length defs)]
    | otherwise = compileLet    compileE env defs body ++ [Slide (length defs)]
compileE env e@(App (Var op) _) = case lookup op unaryOpBox of
    Just instruction -> compileB env e ++ [instruction]
    Nothing          -> compileC env e ++ [Eval]
compileE env e@(App (App (Var op) _) _) = case lookup op binaryOpBox of
    Just instruction -> compileB env e ++ [instruction]
    Nothing          -> compileC env e ++ [Eval]
compileE env e@(App (App (App (Var "if") cond) t) f) =
    compileB env cond ++ [Cond (compileE env t) (compileE env f)]
compileE env (Case e alts) = compileE env e ++ [Casejump $ compileD env compileE alts]
compileE env e = compileC env e ++ [Eval]

-- Compile code for alternatives of a case expression
compileD :: GMEnvironment -> GMCompiler -> [Alt] -> [(Int, GMCode)]
compileD env comp = map $ \(PTag tag, args, expr) ->
    (tag, compileA comp (length args) (zip args [0..] ++ argOffset (length args) env) expr)

-- Parameterized compilation scheme bracketed by Split and Slide
compileA :: GMCompiler -> Int -> GMCompiler
compileA comp offset env expr = [Split offset] ++ comp env expr ++ [Slide offset]

-- Scheme B[e] p compiles code that evaluates an expression e to
-- WHNF in an environment p leaving the result on the V-stack.
compileB :: GMCompiler
compileB env (Num n) = [Pushbasic n]
compileB env (Let recursive defs body)
    | recursive = compileLetrec compileB env defs body ++ [Pop (length defs)]
    | otherwise = compileLet    compileB env defs body ++ [Pop (length defs)]
compileB env e@(App (Var op) e1) = case lookup op unaryOpImpl of
    Just instruction -> compileB env e1 ++ [instruction]
    Nothing          -> compileE env e ++ [Get]
compileB env e@(App (App (Var op) e1) e2) = case lookup op binaryOpImpl of
    Just instruction -> compileB env e2 ++ compileB env e1 ++ [instruction]
    Nothing          -> compileE env e ++ [Get]
compileB env e@(App (App (App (Var "if") cond) t) f) =
    compileB env cond ++ [Cond (compileB env t) (compileB env f)]
compileB env e = compileE env e ++ [Get]

-- Scheme C[e] p generates code which constructs the graph of e
-- in environment p, leaving a pointer to it on top of the stack.
compileC :: GMCompiler
compileC env (Var v) = case lookup v env of
    Just n  -> [Push n]
    Nothing -> [Pushglobal v]
compileC env (Num n) = [Pushint n]
compileC env (App e1 e2) = compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]
compileC env (Cons tag arity) = replicate arity (Push $ arity - 1) ++ [Pack tag arity]
compileC env (Case e alts) = compileE env e ++ [Casejump $ compileD env compileE alts]
compileC env (Let recursive defs body)
    | recursive = compileLetrec compileC env defs body ++ [Slide $ length defs]
    | otherwise = compileLet    compileC env defs body ++ [Slide $ length defs]

-- Generate code to construct each let binding and the let body.
-- Code must remove bindings after body is evaluated.
compileLet :: GMCompiler -> GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLet comp env defs body = compileDefs env defs ++ comp env' body where
    env' = compileArgs env defs

-- Generate code to construct each definition in defs
compileDefs :: GMEnvironment -> [(Name, Expr)] -> GMCode
compileDefs env []                  = []
compileDefs env ((name, expr):defs) = compileC env expr ++ compileDefs (argOffset 1 env) defs

-- Generate code to construct recursive let bindings and the let body.
-- Code must remove bindings after body is evaluated.
-- Bindings start as null pointers and must update themselves on evaluation.
compileLetrec :: GMCompiler -> GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLetrec comp env defs body = [Alloc n] ++ compileRecDefs (n - 1) env' defs ++ comp env' body where
    env' = compileArgs env defs
    n = length defs

-- Generate code to construct each definition in defs and
-- update pointer on stack.
compileRecDefs :: Int -> GMEnvironment -> [(Name, Expr)] -> GMCode
compileRecDefs n env []                  = []
compileRecDefs n env ((name, expr):defs) = compileC env expr ++ [Update n] ++ compileRecDefs (n - 1) env defs

-- Generate stack offsets for local bindings
compileArgs :: GMEnvironment -> [(Name, Expr)] -> GMEnvironment
compileArgs env defs = zip (map fst defs) (reverse [0..n-1]) ++ argOffset n env where
    n = length defs

-- Adjust the stack offsets in the environment by n
argOffset :: Int -> GMEnvironment -> GMEnvironment
argOffset n = map $ second (+n)


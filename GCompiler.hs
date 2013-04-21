module GCompiler (
    compile
) where

import Types
import Heap

import Data.List
import Control.Arrow hiding ((<+>))
import Debug.Trace

-- Standard library
prelude :: Program
prelude = [
    ("I",  ["x"], Var "x"),
    ("K",  ["x", "y"], Var "x"),
    ("K1", ["x", "y"], Var "y"),
    ("S",  ["f", "g", "x"], App (App (Var "f") (Var "x"))
                                (App (Var "g") (Var "x"))),
    ("compose", ["f", "g", "x"], App (Var "f") (App (Var "g") (Var "x"))),
    ("twice",   ["f"], App (App (Var "compose") (Var "f")) (Var "f"))]

-- Extra definitions to add to the initial global environment
extraDefs = []

-- Compiled primitives
prims :: [(Name, Int, GMCode)]
prims =
    [("+",      2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
     ("-",      2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
     ("*",      2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
     ("/",      2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
     ("negate", 1, [Push 1, Eval, Neg, Update 1, Pop 1, Unwind]),
     ("==",     2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
     ("/=",     2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
     ("<",      2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
     ("<=",     2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
     (">",      2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
     (">=",     2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
     ("if",     3, [Push 0, Eval, Casejump [(1, [Split 0, Push 2, Eval, Slide 0]),
                                            (2, [Split 0, Push 1, Eval, Slide 0])],
                    Update 3, Pop 3, Unwind])]

binaryOps :: [(Name, Instruction)]
binaryOps =
    [("+", Add), ("-", Sub), ("*", Mul), ("/", Div),
     ("==", Eq), ("/=", Ne), (">=", Ge),
     (">",  Gt), ("<=", Le), ("<", Lt)]

unaryOps :: [(Name, Instruction)]
unaryOps = [("negate", Neg)]

-- Used for a number of compilation schemes
type GMCompiler = GMEnvironment -> Expr -> GMCode

-- Turn program into initial G-Machine state
compile :: Program -> GMState
compile program = GMState [] codeInit [] [] heap globals statInit where
    (heap, globals) = buildInitialHeap program

-- Push main and unwind
codeInit :: GMCode
codeInit = [Pushglobal "main", Eval, Print]

-- Start at step zero
statInit :: GMStats
statInit = GMStats 0

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

-- Scheme R[e] p d generates code which instantiates the expression
-- e in environment p, for a supercombinator of arity d, and then
-- proceeds to unwind the resulting stack
compileR :: GMCompiler
compileR env e = compileE env e ++ [Update n, Pop n, Unwind] where
    n = length env

-- Scheme E[e] p compiles code that evaluates an expression e to
-- WHNF in environment p, leaving a pointer to the expression on
-- top of the stack.
compileE :: GMCompiler
compileE env (Num n) = [Pushint n]
compileE env (Let recursive defs body)
    | recursive = compileLetrec compileE env defs body
    | otherwise = compileLet    compileE env defs body
compileE env e@(App (Var op) e1) = case lookup op unaryOps of
    Just instruction -> compileE env e1 ++ [instruction]
    Nothing          -> compileC env e ++ [Eval]
compileE env e@(App (App (Var op) e1) e2) = case lookup op binaryOps of
    Just instruction -> compileE env e2 ++ compileE (argOffset 1 env) e1 ++ [instruction]
    Nothing          -> compileC env e ++ [Eval]
compileE env (Case e alts) = compileE env e ++ [Casejump $ compileD env compileE' alts]
compileE env x = compileC env x ++ [Eval]

-- Normal E Scheme bracketed by Split and Slide
compileE' :: Int -> GMCompiler
compileE' offset env expr = [Split offset] ++ compileE env expr ++ [Slide offset]

-- Compile code for alternatives of a case expression
compileD :: GMEnvironment -> (Int -> GMCompiler) -> [Alt] -> [(Int, GMCode)]
compileD env comp = map compileA where
    compileA (tag, args, expr) = (tag, comp (length args) (zip args [0..] ++ argOffset (length args) env) expr)

-- Scheme C[e] p generates code which constructs the graph of e
-- in environment p, leaving a pointer to it on top of the stack.
compileC :: GMCompiler
compileC env (Var v) = case lookup v env of
    Just n  -> [Push n]
    Nothing -> [Pushglobal v]
compileC env (Num n) = [Pushint n]
compileC env (App e1 e2) = compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]
compileC env (Cons tag arity) = replicate arity (Push $ arity - 1) ++ [Pack tag arity]
compileC env (Case e alts) = compileE env e ++ [Casejump $ compileD env compileE' alts]
compileC env (Let recursive defs body)
    | recursive = compileLetrec compileC env defs body
    | otherwise = compileLet    compileC env defs body
compileC env x = error $ "no pattern for " ++ show x ++ "?!"

-- Generate code to construct each let binding and the let body.
-- Code must remove bindings after body is evaluated.
compileLet :: GMCompiler -> GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLet comp env defs body = compileDefs env defs ++ comp env' body ++ [Slide (length defs)] where
    env' = compileArgs env defs

-- Generate code to construct each definition in defs
compileDefs :: GMEnvironment -> [(Name, Expr)] -> GMCode
compileDefs env []                  = []
compileDefs env ((name, expr):defs) = compileC env expr ++ compileDefs (argOffset 1 env) defs

-- Generate code to construct recursive let bindings and the let body.
-- Code must remove bindings after body is evaluated.
-- Bindings start as null pointers and must update themselves on evaluation.
compileLetrec :: GMCompiler -> GMEnvironment -> [(Name, Expr)] -> Expr -> GMCode
compileLetrec comp env defs body = [Alloc n] ++ compileRecDefs (n - 1) env' defs ++ comp env' body ++ [Slide n] where
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


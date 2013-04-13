module Transforms (
    transform
) where

import Types

import qualified Data.Map as Map
import Control.Monad.State

-- Program to Program transformations
transform :: Program -> Program
transform = liftConstructors

-- Map constructor names of the form $Pack{tag,arity} to
-- (tag, arity) for generating supercombinators of the form
-- $Pack{tag,arity} $1 ... $arity = Cons tag arity
type Liftees = Map.Map Name (Int, Int)

-- Lift constructors to top level and add to program as
-- supercombinators
liftConstructors :: Program -> Program
liftConstructors program = buildConstructors (Map.toList constructors) ++ program' where
    (program', constructors) = runState (findConstructors program) Map.empty

-- Build supercombinators for constructors
buildConstructors :: [(Name, (Int, Int))] -> Program
buildConstructors = map buildConstructor where
    buildConstructor (name, (tag, arity)) = (name, makeArgs arity, Cons tag arity)
    makeArgs n = map (('$':) . show) [1..n]

-- Find constructors and replace references to with references to
-- not-yet-created supercombinators
findConstructors :: Program -> State Liftees Program
findConstructors = mapM findConstructor where
    findConstructor (name, args, body) = do
        body' <- walk body
        return $ (name, args, body')

-- Walk down expression tree adding constructors to state and
-- replacing references
walk :: Expr -> State Liftees Expr
walk (App e1 e2) = do
    e1' <- walk e1
    e2' <- walk e2
    return $ App e1' e2'
walk (Cons tag arity) = do
    let name = makeConsName tag arity
    modify $ Map.insert name (tag, arity)
    return $ Var name
walk (Let rec defs expr) = do
    defs' <- walkDefs defs
    expr' <- walk expr
    return $ Let rec defs' expr'
walk (Case expr alts) = do
    expr' <- walk expr
    alts' <- walkAlts alts
    return $ Case expr' alts'
walk (Lambda args expr) = do
    expr' <- walk expr
    return $ Lambda args expr'
walk e = return e

-- Walk each let-binding
walkDefs :: [(Name, Expr)] -> State Liftees [(Name, Expr)]
walkDefs = mapM walkDef where
    walkDef (name, expr) = do
        expr' <- walk expr
        return $ (name, expr')

-- Walk each case-alternative
walkAlts :: [Alt] -> State Liftees [Alt]
walkAlts = mapM walkAlt where
    walkAlt (tag, args, expr) = do
        expr' <- walk expr
        return $ (tag, args, expr')

-- Generate a supercombinator name for constructor
makeConsName :: Int -> Int -> Name
makeConsName tag arity = "$Pack{" ++ show tag ++ "," ++ show arity ++ "}"


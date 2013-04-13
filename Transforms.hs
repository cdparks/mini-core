module Transforms (
    transform
) where

import Types

import qualified Data.Map as Map
import Control.Monad.State

transform :: Program -> Program
transform = liftConstructors

type Liftees = Map.Map Name (Int, Int)

buildConstructors :: [(Name, (Int, Int))] -> Program
buildConstructors []                        = []
buildConstructors ((name, (tag, arity)):xs) = (name, args, Cons tag arity):buildConstructors xs where
    args = map (('$':) . show) [1..arity]

liftConstructors :: Program -> Program
liftConstructors program = buildConstructors (Map.toList constructors) ++ program' where
    (program', constructors) = runState (findConstructors program) Map.empty

findConstructors :: Program -> State Liftees Program
findConstructors [] = return []
findConstructors ((name, args, body):xs) = do
    body' <- walk body
    rest  <- findConstructors xs
    return $ (name, args, body'):rest

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

walkDefs :: [(Name, Expr)] -> State Liftees [(Name, Expr)]
walkDefs []                  = return []
walkDefs ((name, expr):defs) = do
    expr' <- walk expr
    defs' <- walkDefs defs
    return $ (name, expr'):defs'

walkAlts :: [Alt] -> State Liftees [Alt]
walkAlts []                       = return []
walkAlts ((tag, args, expr):alts) = do
    expr' <- walk expr
    alts' <- walkAlts alts
    return $ (tag, args, expr'):alts'

makeConsName :: Int -> Int -> Name
makeConsName tag arity = "$Pack{" ++ show tag ++ "," ++ show arity ++ "}"


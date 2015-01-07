module MiniCore.Transforms.Utils where

import MiniCore.Types
import qualified Data.Set as Set
import Data.List (foldl')

-- Type synonyms for nodes annotated with free variables
type FVProgram = AProgram Name (Set.Set Name)
type FVExpr = Annotated Name (Set.Set Name)
type FVAlt = AAlt Name (Set.Set Name)

-- Walk program and return a new AST annotated with each node's free variables
freeVars :: Program -> Stage FVProgram
freeVars = mapM freeVars'
  where
    freeVars' (Combinator name args body) = do
        body' <- freeVarsExpr (Set.fromList args) body
        return (name, args, body')

-- Annotate expression with free variables
freeVarsExpr :: Set.Set Name -> Expr -> Stage FVExpr
freeVarsExpr vars (Num n) =
    return (Set.empty, ANum n)

freeVarsExpr vars (Cons tag arity) =
    return (Set.empty, ACons tag arity)

freeVarsExpr vars (Var v)
    | v `Set.member` vars = return (Set.singleton v, AVar v)
    | otherwise           = return (Set.empty, AVar v)

freeVarsExpr vars (BinOp op e1 e2) =
    freeVarsExpr vars (App (App (Var op) e1) e2)

freeVarsExpr vars (App e1 e2) = do
    e1' <- freeVarsExpr vars e1
    e2' <- freeVarsExpr vars e2
    let vars' = freeVarsOf e1' `Set.union` freeVarsOf e2'
    return (vars', AApp e1' e2')

freeVarsExpr vars (Lambda args body) = do
    let argSet = Set.fromList args
    body' <- freeVarsExpr (vars `Set.union` argSet) body
    let vars' = freeVarsOf body' `Set.difference` argSet
    return (vars', ALambda args body')

freeVarsExpr vars (Case expr alts) = do
    expr' <- freeVarsExpr vars expr
    alts' <- mapM (freeVarsAlt vars) alts
    let vars' = freeVarsOf expr' `Set.union` (Set.unions $ map freeVarsOfAlt alts')
    return (vars', ACase expr' alts')

freeVarsExpr vars (Let recursive defs body) = do
    let binders = bindersOf defs
        binderSet = Set.fromList binders
        bodyVars = vars `Set.union` binderSet
        defVars
           | recursive = bodyVars
           | otherwise = vars
    exprs <- mapM (freeVarsExpr defVars) $ bindeesOf defs
    let defs' = zip binders exprs
        freeInValues = Set.unions $ map freeVarsOf exprs
        defsFree
           | recursive = freeInValues `Set.difference` binderSet
           | otherwise = freeInValues
    body' <- freeVarsExpr bodyVars body
    let bodyFree = freeVarsOf body' `Set.difference` binderSet
        vars' = defsFree `Set.union` bodyFree
    return (vars', ALet recursive defs' body')

-- Annotate alternative with free vars
freeVarsAlt :: Set.Set Name -> Alt -> Stage FVAlt
freeVarsAlt vars (tag, args, expr) = do
    let argSet = Set.fromList args
    expr' <- freeVarsExpr (vars `Set.union` argSet) expr
    return (tag, args, expr')

-- Just get free variables for annotated AST node
freeVarsOf :: Annotated Name (Set.Set Name) -> Set.Set Name
freeVarsOf (vars, _) = vars

-- Just get free variables for alternative
freeVarsOfAlt :: FVAlt -> Set.Set Name
freeVarsOfAlt (tag, args, rhs) = freeVarsOf rhs `Set.difference` Set.fromList args


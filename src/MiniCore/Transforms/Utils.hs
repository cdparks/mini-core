module MiniCore.Transforms.Utils (
    freeVars,
    freeVarsOf
) where

import MiniCore.Types
import qualified Data.Set as Set

-- Walk program and return a new AST annotated with each node's free variables
freeVars :: Program -> AProgram Name (Set.Set Name)
freeVars = map $ \(Combinator name args body) -> (name, args, walk (Set.fromList args) body) where
    walk :: Set.Set Name -> Expr -> Annotated Name (Set.Set Name)
    walk vars (Num n) = (Set.empty, ANum n)
    walk vars (Cons tag arity) = (Set.empty, ACons tag arity)
    walk vars (Var v)
        | v `Set.member` vars = (Set.singleton v, AVar v)
        | otherwise           = (Set.empty, AVar v)
    walk vars (App e1 e2) = (vars', AApp e1' e2') where
        e1' = walk vars e1
        e2' = walk vars e2
        vars' = freeVarsOf e1' `Set.union` freeVarsOf e2'
    walk vars (Lambda args body) = (vars', ALambda args body') where
        argSet = Set.fromList args
        body' = walk (vars `Set.union` argSet) body
        vars' = freeVarsOf body' `Set.difference` argSet
    walk vars (Case expr alts) = (vars', ACase expr' alts') where
        expr' = walk vars expr
        alts' = map (walkAlt vars) alts
        vars' = freeVarsOf expr' `Set.union` (flattenSet $ map freeVarsOfAlt alts')
    walk vars (Let recursive defs body) = (vars', ALet recursive defs' body') where
        binders = bindersOf defs
        binderSet = Set.fromList binders
        bodyVars = vars `Set.union` binderSet
        rhsVars | recursive = bodyVars
                | otherwise = vars
        rhs' = map (walk rhsVars) $ bindeesOf defs
        defs' = zip binders rhs'
        freeInValues = flattenSet $ map freeVarsOf rhs'
        defsFree | recursive = freeInValues `Set.difference` binderSet
                 | otherwise = freeInValues
        body' = walk bodyVars body
        bodyFree = freeVarsOf body' `Set.difference` binderSet
        vars' = defsFree `Set.union` bodyFree

    flattenSet :: Ord a => [Set.Set a] -> Set.Set a
    flattenSet = foldl Set.union Set.empty

    freeVarsOfAlt :: AAlt Name (Set.Set Name) -> Set.Set Name
    freeVarsOfAlt (tag, args, rhs) = freeVarsOf rhs `Set.difference` Set.fromList args

    walkAlt :: Set.Set Name -> Alt -> AAlt Name (Set.Set Name)
    walkAlt vars (tag, args, rhs) = (tag, args, rhs') where
        argSet = Set.fromList args
        rhs' = walk (vars `Set.union` argSet) rhs

-- Just get free variables for annotated AST node
freeVarsOf :: Annotated Name (Set.Set Name) -> Set.Set Name
freeVarsOf (vars, _) = vars


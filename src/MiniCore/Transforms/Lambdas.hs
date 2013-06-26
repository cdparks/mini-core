module MiniCore.Transforms.Lambdas (
    liftLambdas
) where

import MiniCore.Types

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (partition)

-- Lift lambdas to top level as supercombinators and turn free variables into
-- extra formal parameters
liftLambdas :: Program -> Program
liftLambdas = collectCombinators . rename . abstract . freeVars

-- Wrap an integer used to generate new names
data NameSupply = NameSupply { suffix :: Int }

-- Generate a new name from the NameSupply
fresh :: State NameSupply Name
fresh = do
    n <- gets suffix
    modify $ \s -> s { suffix = n + 1 }
    return $ "$" ++ show n

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

-- Partially apply lambda expressions to pass in "free" variables
abstract :: AProgram Name (Set.Set Name) -> Program
abstract = map $ \(name, args, rhs) -> Combinator name args $ walk rhs where
    walk :: Annotated Name (Set.Set Name) -> Expr
    walk (free, AVar v) = Var v
    walk (free, ANum n) = Num n
    walk (free, ACons tag arity) = Cons tag arity
    walk (free, AApp e1 e2) = App (walk e1) (walk e2)
    walk (free, ALet recursive defs body) = Let recursive defs' body' where
        defs' = zip (bindersOf defs) $ map walk $ bindeesOf defs
        body' = walk body
    walk (free, ACase expr alts) = Case expr' alts' where
        walkAlt (tag, args, rhs) = (tag, args, walk rhs)
        expr' = walk expr
        alts' = map walkAlt alts
    walk (free, ALambda args body) = foldl App sc $ map Var vars where
        vars = Set.toList free
        sc = Let False [("sc", rhs)] (Var "sc")
        rhs = Lambda (vars ++ args) $ walk body

-- Make each name in program unique
rename :: Program -> Program
rename program = evalState (mapM renameSC program) $ NameSupply 1 where
    renameSC (Combinator name args body) = do
        (args', env) <- newNames args
        body' <- walk env body
        return $ Combinator name args' body'

    walk :: Map.Map Name Name -> Expr -> State NameSupply Expr
    walk env (Var v) = return $ case Map.lookup v env of
        Just x  -> Var x
        Nothing -> Var v
    walk env x@(Num _) = return x
    walk env x@(Cons _ _) = return x
    walk env (App e1 e2) = do
        e1' <- walk env e1
        e2' <- walk env e2
        return $ App e1' e2'
    walk env (Lambda args body) = do
        (args', env') <- newNames args
        body' <- walk (env' `Map.union` env) body
        return $ Lambda args' body'
    walk env (Let recursive defs body) = do
        let binders = bindersOf defs
        (binders', env') <- newNames binders
        let bodyEnv = env' `Map.union` env
        body' <- walk bodyEnv body
        let rhsEnv | recursive = bodyEnv
                   | otherwise = env
        rhs' <- mapM (walk rhsEnv) $ bindeesOf defs
        return $ Let recursive (zip binders' rhs') body'
    walk env (Case expr alts) = do
        expr' <- walk env expr
        alts' <- mapM (walkAlt env) alts
        return $ Case expr' alts'

    walkAlt :: Map.Map Name Name -> Alt -> State NameSupply Alt
    walkAlt env (tag, args, rhs) = do
        (args', env') <- newNames args
        rhs' <- walk (env' `Map.union` env) rhs
        return $ (tag, args', rhs')

-- Take a list of old names and return a list of new names and a mapping
-- from the old names to the new names
newNames :: [Name] -> State NameSupply ([Name], Map.Map Name Name)
newNames args = do
    args' <- replicateM (length args) fresh
    return (args', Map.fromList $ zip args args')

-- Find lambda expressions and promote them to supercombinators
collectCombinators :: Program -> Program
collectCombinators = concatMap $ \c -> execState (collectCombinator c) [] where
    collectCombinator :: Declaration -> State [Declaration] ()
    collectCombinator (Combinator name args body) = do
        body' <- walk body
        modify $ \cs -> Combinator name args body':cs

    walk :: Expr -> State [Declaration] Expr
    walk (App e1 e2) = do
        e1' <- walk e1
        e2' <- walk e2
        return $ App e1' e2'
    walk (Lambda args body) = do
        body' <- walk body
        return $ Lambda args body'
    walk (Case expr alts) = do
        expr' <- walk expr
        alts' <- mapM walkAlt alts
        return $ Case expr' alts'
    walk (Let recursive defs body) = do
        defs' <- mapM walkDef defs
        body' <- walk body
        let (combinators, bindings) = partition (isLambda . snd) defs'
            lifted                  = map toCombinator combinators
        modify $ \cs -> lifted ++ cs
        case bindings of
            [] -> return body'
            _  -> return $ Let recursive bindings body'
    walk x = return x

    walkDef :: (Name, Expr) -> State [Declaration] (Name, Expr)
    walkDef (name, expr) = do
        expr' <- walk expr
        return (name, expr')

    walkAlt :: Alt -> State [Declaration] Alt
    walkAlt (tag, args, rhs) = do
        rhs' <- walk rhs
        return $ (tag, args, rhs')

    isLambda :: Expr -> Bool
    isLambda (Lambda _ _) = True
    isLambda _            = False

    toCombinator :: (Name, Expr) -> Declaration
    toCombinator (name, Lambda args body) = Combinator name args body


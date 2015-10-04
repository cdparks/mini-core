module MiniCore.Transforms.Lambdas
  ( liftLambdas
  ) where

import MiniCore.Types
import MiniCore.Transforms.Utils

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.List (partition, foldl')

-- Lift lambdas to top level as supercombinators and turn free variables into
-- extra formal parameters
liftLambdas :: Program -> Stage Program
liftLambdas program = collectCombinators =<< rename =<< abstract =<< freeVars program

-- Partially apply lambda expressions to pass in "free" variables
abstract :: FVProgram -> Stage Program
abstract = mapM abstract'
 where
  abstract' (name, args, body) =
    Combinator name args <$> walk body

  walk :: FVExpr -> Stage Expr
  walk (free, AVar v) =
    return (Var v)

  walk (free, ANum n) =
    return (Num n)

  walk (free, ACons tag arity) =
    return (Cons tag arity)

  walk (free, AApp e1 e2) =
    App <$> walk e1 <*> walk e2

  walk (free, ALet recursive defs body) = do
    let names = bindersOf defs
    exprs <- mapM walk (bindeesOf defs)
    body' <- walk body
    return (Let recursive (zip names exprs) body')

  walk (free, ACase expr alts) =
    let walkAlt (tag, args, rhs) = (,,) tag args <$> walk rhs
    in Case <$> walk expr <*> mapM walkAlt alts

  walk (free, ALambda args body) = do
    body' <- walk body
    let vars = Set.toList free
        rhs  = Lambda (vars ++ args) body'
        sc   = Let False [(lambda, rhs)] (Var lambda)
    return (foldl' App sc (map Var vars))

-- Wrap an integer used to generate new names
data NameSupply = NameSupply { suffix :: Int }

-- Keep track of name supply and be able to throw an error
type Lift a = StateT NameSupply Stage a

-- Generate a new name from the NameSupply
fresh :: Name -> Lift Name
fresh name = do
  n <- gets suffix
  modify (\s -> s { suffix = n + 1 })
  return (name ++ "_$" ++ show n)

-- Prefix for lifted lambdas
lambda = "$lambda"

-- Do we need to rename this variable?
shouldReplace :: Map.Map Name Name -> Name -> Bool
shouldReplace env name = name `Map.member` env || name == lambda

-- Take a list of old names and return a list of new names and a mapping
-- from the old names to the new names
newNames :: Map.Map Name Name -> [Name] -> Lift ([Name], Map.Map Name Name)
newNames env []     = return ([], env)
newNames env (x:xs) = do
  arg <-
    if shouldReplace env x
      then fresh x
      else return x
  let env' = Map.insert x arg env
  (args, env') <- newNames env' xs
  return (arg:args, env')

-- Rename variables in the program that might clash
rename :: Program -> Stage Program
rename program = evalStateT (mapM renameSC program) $ NameSupply 1
 where
  renameSC :: Declaration -> Lift Declaration
  renameSC (Combinator name args body) = do
    (args', env) <- newNames Map.empty args
    body' <- walk env body
    return (Combinator name args' body')

  walk :: Map.Map Name Name -> Expr -> Lift Expr
  walk env (Var v) = case Map.lookup v env of
    Just x  -> return (Var x)
    Nothing -> return (Var v)

  walk env x@(Num _) =
    return x

  walk env x@(Cons _ _) =
    return x

  walk env (App e1 e2) =
    App <$> walk env e1 <*> walk env e2

  walk env (Lambda args body) = do
    (args', env') <- newNames env args
    body' <- walk (env' `Map.union` env) body
    return (Lambda args' body')

  walk env (Let recursive defs body) = do
    let binders = bindersOf defs
    (binders', env') <- newNames env binders
    let bodyEnv = env' `Map.union` env
    body' <- walk bodyEnv body
    let rhsEnv
          | recursive = bodyEnv
          | otherwise = env
    rhs' <- mapM (walk rhsEnv) (bindeesOf defs)
    return (Let recursive (zip binders' rhs') body')

  walk env (Case expr alts) =
    Case <$> walk env expr <*> mapM (walkAlt env) alts

  walkAlt :: Map.Map Name Name -> Alt -> Lift Alt
  walkAlt env (tag, args, rhs) = do
    (args', env') <- newNames env args
    rhs' <- walk (env' `Map.union` env) rhs
    return $ (tag, args', rhs')

-- Keep track of new combinators
type Collect a = StateT [Declaration] Stage a

-- Find lambda expressions and promote them to supercombinators
collectCombinators :: Program -> Stage Program
collectCombinators = liftM concat . mapM go
 where
  go :: Declaration -> Stage Program
  go c = execStateT (collectCombinator c) []

  collectCombinator :: Declaration -> Collect ()
  collectCombinator (Combinator name args body) = do
    body' <- walk body
    modify (\cs -> Combinator name args body':cs)

  walk :: Expr -> Collect Expr
  walk (App e1 e2) =
    App <$> walk e1 <*> walk e2

  walk (Lambda args body) =
    Lambda args <$> walk body

  walk (Case expr alts) =
    Case <$> walk expr <*> mapM walkAlt alts

  walk (Let recursive defs body) = do
    defs' <- mapM walkDef defs
    body' <- walk body
    let (combinators, bindings) = partition (isLambda . snd) defs'
        lifted                  = map toCombinator combinators
    modify (\cs -> lifted ++ cs)
    case bindings of
        [] -> return body'
        _  -> return $ Let recursive bindings body'

  walk x = return x

  walkDef :: (Name, Expr) -> Collect (Name, Expr)
  walkDef (name, expr) =
    (,) name <$> walk expr

  walkAlt :: Alt -> Collect Alt
  walkAlt (tag, args, rhs) =
    (,,) tag args <$> walk rhs

  isLambda :: Expr -> Bool
  isLambda (Lambda _ _) = True
  isLambda _            = False

  toCombinator :: (Name, Expr) -> Declaration
  toCombinator (name, Lambda args body) = Combinator name args body


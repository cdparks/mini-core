module MiniCore.Transforms.Constructors (
    transformConstructors
) where

import MiniCore.Types

import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

-- Maintain mapping of constructors to arity and tag
-- and allow errors to propagate
type Gen a = StateT ConstructorState Stage a

-- Generate constructor combinators and use tags in case-expressions
transformConstructors :: Program -> Stage Program
transformConstructors program = evalStateT run initialEnv
  where
    run = do program' <- replaceDataDecls program
             convertCases program'

-- Strip out Data declarations and convert their constructors into super-combinators
-- Return new program and mapping from Constructor names to tags
replaceDataDecls :: Program -> Gen Program
replaceDataDecls program = 
    do let (dataDecls, combinators) = partition isData program
           constructors = concatMap getConstructors dataDecls
       combinators' <- mapM newCombinator constructors
       return $ combinators' ++ combinators

-- After all the Data declarations have been converted, use the ConstructorEnv
-- to convert case expressions from constructors to tags
convertCases :: Program -> Gen Program
convertCases program = mapM convert program
  where
    convert (Combinator name args body) =
        do body' <- walk body
           return $ Combinator name args body'

-- Grab all constructors out of a single Data declaration
getConstructors :: Declaration -> [Constructor]
getConstructors (Data _ _ constructors) = constructors
getConstructors _                       = [] -- Shouldn't happen

-- Map constructor names to (tag, arity)
type ConstructorEnv = Map.Map Name (Int, Int)
type ConstructorTag = Int

-- Map constructor names to (tag, arity). Increment the tag when
-- adding a new constructor.
data ConstructorState = ConstructorState
    { cTag :: ConstructorTag
    , cEnv :: ConstructorEnv
    } deriving Show

-- The default constructor _ has tag and arity 0. Additionally, True and False
-- are defined in the prelude since primitives can generate them.
initialEnv = ConstructorState
    { cTag = 3
    , cEnv = Map.fromList
        [ ("_", (0, 0))
        , ("False", (1, 0))
        , ("True", (2, 0))
        ]
    }

-- Generate a new constructor combinator and associate a new tag
-- with the constructor name. Raise error on duplicate constructor
-- names.
newCombinator :: Constructor -> Gen Declaration
newCombinator (Constructor name components) = do
    env <- gets cEnv
    tag <- gets cTag
    let arity = length components
        args  = map (("$x"++) . show) [1..arity]
    when (Map.member name env) $
        error $ "Duplicate constructor " ++ name
    modify $ \s -> s
        { cTag = tag + 1
        , cEnv = Map.insert name (tag, arity) env
        }
    return $ Combinator name args
           $ foldl App (Cons tag arity)
           $ map Var args

-- Walk each combinator body and replace Constructor names in
-- case expressions with integer tags
walk :: Expr -> Gen Expr
walk (App e1 e2) = do
    e1' <- walk e1
    e2' <- walk e2
    return $ App e1' e2'
walk (Let recursive bindings body) = do
    bindings' <- walkBindings bindings
    body'     <- walk body
    return $ Let recursive bindings' body'
walk (Lambda args body) = do
    body' <- walk body
    return $ Lambda args body
walk (Case body alts) = do
    alts' <- walkAlts alts
    body' <- walk body
    return $ Case body' alts'
walk x = return x

-- Replace Constructor names in let-bindings
walkBindings :: [(Name, Expr)] -> Gen [(Name, Expr)]
walkBindings bindings = do
    let (names, exprs) = unzip bindings
    exprs' <- mapM walk exprs
    return $ zip names exprs'

-- Replace Constructor names in case alternatives
walkAlts :: [Alt] -> Gen [Alt]
walkAlts alts = mapM walkAlt alts where
    walkAlt (PCon constructor, args, expr) = do
        env <- gets cEnv
        (tag, arity) <- case Map.lookup constructor env of
                            Just x  -> return x
                            Nothing -> throwError $ "No declaration found for " ++ constructor
        expr' <- if length args /= arity then
                    throwError $ "Constructor " ++ constructor ++
                                 " has " ++ show arity ++ " components"
                 else walk expr
        return (PTag tag, args, expr')


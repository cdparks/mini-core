module MiniCore.Transforms.Constructors (
    transformConstructors
) where

import MiniCore.Types

import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

-- Generate constructor combinators and use tags in case-expressions
transformConstructors :: Program -> Program
transformConstructors = convertCases . replaceDataSpecs

-- Strip out DataSpecs and convert their constructors into super-combinators
-- Return new program and mapping from Constructor names to tags
replaceDataSpecs :: Program -> (Program, ConstructorEnv)
replaceDataSpecs program = (combinators' ++ combinators, cEnv state) where
    (dataSpecs, combinators) = partition isDataSpec program
    constructors = concatMap getConstructors dataSpecs
    (combinators', state) = runState (mapM newCombinator constructors) initialEnv

-- After all the DataSpecs have been converted, use the ConstructorEnv
-- to convert case expressions from constructors to tags
convertCases :: (Program, ConstructorEnv) -> Program
convertCases (program, env) = map convert program where
    convert (Combinator name args body) = Combinator name args $
                                            runReader (walk body) env

-- Is declaration a DataSpec or a Combinator?
isDataSpec :: Declaration -> Bool
isDataSpec (DataSpec {}) = True
isDataSpec _             = False

-- Grab all constructors out of a single DataSpec
getConstructors :: Declaration -> [Constructor]
getConstructors (DataSpec _ constructors) = constructors
getConstructors _                         = [] -- Shouldn't happen

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
newCombinator :: Constructor -> State ConstructorState Declaration
newCombinator (name, components) = do
    env <- gets cEnv
    tag <- gets cTag
    let arity = length components
    when (Map.member name env) $
        error $ "Duplicate constructor " ++ name
    modify $ \s -> s
        { cTag = tag + 1
        , cEnv = Map.insert name (tag, arity) env
        }
    return $ Combinator name components
           $ foldl App (Cons tag arity)
           $ map Var components

-- Walk each combinator body and replace Constructor names in
-- case expressions with integer tags
walk :: Expr -> Reader ConstructorEnv Expr
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
walkBindings :: [(Name, Expr)] -> Reader ConstructorEnv [(Name, Expr)]
walkBindings bindings = do
    let (names, exprs) = unzip bindings
    exprs' <- mapM walk exprs
    return $ zip names exprs'

-- Replace Constructor names in case alternatives
walkAlts :: [Alt] -> Reader ConstructorEnv [Alt]
walkAlts alts = mapM walkAlt alts where
    walkAlt (PCon constructor, args, expr) = do
        value <- asks $ Map.lookup constructor
        let (tag, arity) = case value of
                Just x  -> x
                Nothing -> error $ "No declaration found for " ++ constructor
        expr' <- if length args /= arity then
                    error $ "Constructor " ++ constructor ++
                            " has " ++ show arity ++ " components"
                 else walk expr
        return (PTag tag, args, expr')


module MiniCore.Transforms.Constructors (
    transformConstructors
) where

import MiniCore.Types

import Data.List
import Data.Function
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

-- Maintain mapping of constructors to arity and tag
-- and allow errors to propagate
type Gen a = StateT ConstructorState Stage a

-- Generate constructor combinators and use tags in case-expressions
transformConstructors :: Program -> Stage ([Name], Program)
transformConstructors program = do
    (program', state) <- runStateT run initialEnv
    return (toCons $ cEnv state, program')
  where
    run = replaceDataDecls program >>= convertCases

-- Strip out Data declarations and convert their constructors into super-combinators
-- Return new program and mapping from Constructor names to tags
replaceDataDecls :: Program -> Gen Program
replaceDataDecls program = do
    let (dataDecls, combinators) = partition isData program
        constructors = concatMap getConstructors dataDecls
    combinators' <- mapM newCombinator constructors
    return $ combinators' ++ combinators

-- After all the Data declarations have been converted, use the ConstructorEnv
-- to convert case expressions from constructors to tags
convertCases :: Program -> Gen Program
convertCases = mapM convert
  where
    convert (Combinator name args body) =
        Combinator name args <$> convertExpr body

-- Grab all constructors out of a single Data declaration
getConstructors :: Declaration -> [Constructor]
getConstructors (Data _ _ constructors) = constructors
getConstructors _                       = [] -- Shouldn't happen

-- Map constructor names to (tag, arity)
type ConstructorEnv = Map.Map Name (Int, Int)
type ConstructorTag = Int

-- Get constructors in tag order
toCons :: ConstructorEnv -> [Name]
toCons = map fst . sortBy (compare `on` snd) . Map.toList

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
       typeError ("Duplicate constructor " ++ name)
    modify $ \s -> s
        { cTag = tag + 1
        , cEnv = Map.insert name (tag, arity) env
        }
    return $ Combinator name args
           $ foldl' App (Cons tag arity)
           $ map Var args

-- Walk each combinator body and replace Constructor names in
-- case expressions with integer tags
convertExpr :: Expr -> Gen Expr
convertExpr (App e1 e2) =
    App <$> convertExpr e1 <*> convertExpr e2
convertExpr (Let recursive bindings body) =
    Let recursive <$> convertBindings bindings <*> convertExpr body
convertExpr (Lambda args body) =
    Lambda args <$> convertExpr body
convertExpr (Case body alts) =
    Case <$> convertExpr body <*> convertAlts alts
convertExpr x = return x

-- Replace Constructor names in let-bindings
convertBindings :: [(Name, Expr)] -> Gen [(Name, Expr)]
convertBindings bindings = do
    let (names, exprs) = unzip bindings
    exprs' <- mapM convertExpr exprs
    return $ zip names exprs'

-- Replace Constructor names in case alternatives
convertAlts :: [Alt] -> Gen [Alt]
convertAlts = mapM convertAlt
  where
    convertAlt (PCon constructor, args, expr) = do
        env <- gets cEnv
        (tag, arity) <- case Map.lookup constructor env of
                Just x  -> return x
                Nothing -> typeError ("No declaration found for " ++ constructor)
        expr <- convertExpr expr
        return (PTag tag, args, expr)


module MiniCore.Transforms.Constructors (
    convertConstructors
) where

import MiniCore.Types

import Data.List
import qualified Data.Map as Map
import Control.Monad.State

-- Strip out DataSpecs and convert their constructors into super-combinators
-- Return new program and 
convertConstructors :: Program -> (Program, ConstructorEnv)
convertConstructors program = (combinators' ++ combinators, cEnv state) where
    (dataSpecs, combinators) = partition isDataSpec program
    constructors = concatMap getConstructors dataSpecs
    (combinators', state) = runState (mapM newCombinator constructors) initialEnv

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


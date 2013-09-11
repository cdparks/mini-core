{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MiniCore.Inference (
    typecheck,
) where 

import MiniCore.Types
import MiniCore.Format
import MiniCore.Transforms.StronglyConnectedComponents

import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

-- Public compiler stage
typecheck :: Program -> Stage ([(Name, Scheme)], Program)
typecheck = runTI . inferTypes

{- Initial type-environment with primitive operations -}
primOps = 
    [ ("if",    Scheme ["a"] (boolTy `arrow` (var "a" `arrow` (var "a" `arrow` var "a"))))
    , ("+",     Scheme []    (intTy  `arrow` (intTy  `arrow` intTy)))
    , ("-",     Scheme []    (intTy  `arrow` (intTy  `arrow` intTy)))
    , ("*",     Scheme []    (intTy  `arrow` (intTy  `arrow` intTy)))
    , ("/",     Scheme []    (intTy  `arrow` (intTy  `arrow` intTy)))
    , ("-",     Scheme []    (intTy  `arrow` (intTy  `arrow` intTy)))
    , ("||",    Scheme []    (boolTy `arrow` (boolTy `arrow` boolTy)))
    , ("&&",    Scheme []    (boolTy `arrow` (boolTy `arrow` boolTy)))
    , ("==",    Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , ("/=",    Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , ("<",     Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , (">",     Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , ("<=",    Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , (">=",    Scheme []    (intTy  `arrow` (intTy  `arrow` boolTy)))
    , ("True",  Scheme []     boolTy)
    , ("False", Scheme []     boolTy)
    ]

{- Internal types for type-checking -}

-- Internal type-inference state
data TIState = TIState
    { tiDecl :: Maybe Declaration    -- Typechecking Context
    , tiNext :: Int                  -- Generate fresh type variables
    , tiCons :: TypeEnv              -- Constructor for case-expressions
    , tiTEnv :: TypeEnv              -- Named entities to show types for
    , tiData :: Map.Map Name Type    -- Available types
    }

-- Keep track of type-inference state and be able to throw an error
--type TI a = ErrorT String (StateT TIState Identity) a
--type TI a = ErrorT String (StateT TIState IO) a
type TI a = StateT TIState Stage a

{- Manipulating TI context -} 

-- Set declaration as current typechecking context
setContext :: Declaration -> TI ()
setContext decl =
    do modify $ \s -> s { tiDecl = Just decl }

-- Make context declaration out of name and expression
setNamedExpr :: Name -> Expr -> TI ()
setNamedExpr name expr =
    do let decl = case expr of
             Lambda args body -> Combinator name args body
             _                -> Combinator name [] expr
       setContext decl

-- Save types of named values in type environment
saveEnv :: TypeEnv -> [Name] -> TI ()
saveEnv env' names =
    do env <- gets tiTEnv
       let specified = fromKeys env' names
       modify $ \s -> s { tiTEnv = specified `Map.union` env }

-- Throw an error after adding context information
raise :: String -> TI a
raise msg =
    do decl <- gets tiDecl
       let error = case decl of
             Just decl -> msg ++ "\nin:\n\t" ++ show (format decl)
             Nothing   -> msg
       throwError $ "Type Error:\n\t" ++ error

{- Utilities -}

-- Used to quote entities we're printing
quote :: String -> String
quote s = "'" ++ s ++ "'"

-- Make a new Map just using the specified keys
fromKeys m []     = Map.empty
fromKeys m (x:xs) =
    case Map.lookup x m of
        Just v  -> Map.insert x v (fromKeys m xs)
        Nothing -> fromKeys m xs

{- Unification -}

-- Identity substition has no effect when applied
idSubst :: Subst
idSubst = Map.empty

-- If constraint is non-trivial and passes the occurs check,
-- add it to substitution
extend :: Subst -> Name -> Type -> TI Subst
extend s n t
    | t == TVar n            = return s
    | n `Set.member` tvars t = raise "Cannot construct infinite type"
    | otherwise              = return $ Map.singleton n t `scomp` s

-- Find a substituion that unifies pair of types.
unify :: Subst -> (Type, Type) -> TI Subst
unify s (TVar n, t)
    | n' == TVar n = extend s n t'
    | otherwise    = unify s (n', t')
  where
    n' = apply s (TVar n)
    t' = apply s t
unify s (TCon n ts, TVar n') =
    unify s (TVar n', TCon n ts)
unify s (TCon n ts, TCon n' ts')
    | n == n'   = unifyAll s (ts `zip` ts')
    | otherwise = raise $ "Cannot unify " ++ quote n ++ " and " ++ quote n'

-- Unify each pair of types propagating the substition
unifyAll :: Subst -> [(Type, Type)] -> TI Subst
unifyAll = foldM unify

{- Generating new type variables -}

-- Get a new name
fresh :: TI Name
fresh =
    do n <- gets tiNext
       modify $ \s -> s { tiNext = n + 1 }
       return $ "t" ++ show n

-- Given a list of names, return a substitution that maps
-- the old names to completely new ones
withNames :: [Name] -> TI Subst
withNames = foldM newName Map.empty
  where
    newName env name =
        do x <- fresh
           return $ Map.insert name (TVar x) env

{- Type checking for data declarations -}

-- Add a new data type to the tiData mapping
newDataType :: Name -> Type -> TI ()
newDataType name t =
    do types <- gets tiData
       when (name `Map.member` types) $
           raise $ "Redeclaration of data type " ++ quote name
       modify $ \s -> s { tiData = Map.insert name t types }

-- Ensure each data declaration is well-formed:
-- * Must not be redeclaration
-- * Each constructor must be well-formed
-- * Type variables are replaced with fresh type variables
checkDataTypes :: TypeEnv -> [Declaration] -> TI TypeEnv
checkDataTypes = foldM checkDataType where
    checkDataType env decl@(Data name vars constructors) =
        do setContext decl
           s <- withNames vars
           let ty       = apply s $ TCon name $ map TVar vars
               vars'    = tvars ty
               mkScheme = Scheme (Set.toList vars') . foldr arrow ty . apply s
           newDataType name ty
           checkConstructors mkScheme (Set.fromList vars) env constructors

-- Ensure each constructor for a data type is well-formed
-- * Must not be a redeclaration
-- * Type-variables must be in scope
-- * Composite constructors must use already-defined types
checkConstructors :: ([Type] -> Scheme) -> Set.Set Name -> TypeEnv -> [Constructor] -> TI TypeEnv
checkConstructors mkScheme vars = foldM checkConstructor where
    checkConstructor env (Constructor name args) =
        do when (not (tvars args `Set.isSubsetOf` vars)) $
               raise $ "Unbound type variable in constructor " ++ quote name
           cons <- gets tiCons
           when (name `Map.member` cons) $
               raise $ "Redeclaration of constructor " ++ quote name
           mapM_ validType args
           let scheme = mkScheme args
           modify $ \s -> s { tiCons = Map.insert name scheme cons }
           return $ Map.insert name scheme env

    validType (TVar _) = return ()
    validType (TCon n ts) =
        do mapM_ validType ts
           types <- gets tiData
           when (n `Map.notMember` types) $
               raise $ "Use of undeclared type " ++ quote n
           return ()

{- Type checking for bindings and expressions -}

-- Type check a list of expressions producing a composite substition
-- and a list of types
tcAll :: TypeEnv -> [(Name, Expr)] -> TI (Subst, [Type])
tcAll env []                = return (idSubst, [])
tcAll env ((name, expr):es) =
    do setNamedExpr name expr
       (s, t)   <- tcExpr env expr
       (s', ts) <- tcAll (apply s env) es
       return (s' `scomp` s, apply s' t : ts)

-- Type-check an expression in the current type-environment producing
-- a substitution and a concrete type
tcExpr :: TypeEnv -> Expr -> TI (Subst, Type)

-- Literal integer
tcExpr env (Num _) = return (idSubst, intTy)

-- Variable
tcExpr env (Var x) =
    do scheme <- case Map.lookup x env of
           Nothing     -> raise $ "Unbound variable " ++ quote x
           Just scheme -> return scheme
       t <- newInstance scheme
       return (idSubst, t)

-- Application 
tcExpr env (App e1 e2) =
    do (s1, t1) <- tcExpr env e1
       (s2, t2) <- tcExpr env e2
       let t1' = apply s2 t1
           s   = s2 `scomp` s1
       n  <- fresh
       s' <- unify s (t1', t2 `arrow` TVar n)
       return (s', apply s' (TVar n))

-- Binary application
tcExpr env (BinOp op lhs rhs) = tcExpr env (App (App (Var op) lhs) rhs)

-- Lambda
tcExpr env (Lambda args e) =
    do schemes <- toSchemes args
       (s, t) <- tcExpr (schemes `Map.union` env) e
       let schemes' = apply s schemes
           args' = fromSchemes schemes' args
       return (s, foldr arrow t args')

-- Case
tcExpr env (Case scrutinee alts) =
    do (s, t) <- tcExpr env scrutinee
       (s', t') <- tcAlts env t alts
       return (s' `scomp` s, t')

-- Non-recursive Let
tcExpr env (Let False bindings expr) =
    do let (names, exprs) = unzip bindings

       -- Infer monomorphic types for definitions
       (s, ts) <- tcAll env bindings

       -- Generalize types and add to type-environment
       env'' <- addDecls (apply s env) names ts

       -- Save types of named values
       saveEnv  env'' names

       -- Infer type for body
       (s', t) <- tcExpr env'' expr
       return (s' `scomp` s, t)

-- Recursive Let
tcExpr env (Let True bindings expr) =
    do let (names, exprs) = unzip bindings

       -- Add new type-variables for definitions
       schemes <- toSchemes names

       -- Infer monomorphic types for definitions
       (s, ts) <- tcAll (schemes `Map.union` env) bindings

       -- Apply substition to type-variables and unify
       -- with inferred types
       let schemes' = apply s schemes
           env'     = apply s env
           ts'      = fromSchemes schemes' names
       s' <- unifyAll s (zip ts ts')
       let ts'' = fromSchemes (apply s' schemes') names

       -- Generalize types and add to environment
       env'' <- addDecls (apply s' env') names ts''

       -- Save types of named values
       saveEnv env'' names

       --Infer type for body
       (s'', t) <- tcExpr env'' expr
       return (s'' `scomp` s', t)

-- Type-check each alternative in a case-expression
tcAlts :: TypeEnv -> Type -> [Alt] -> TI (Subst, Type)
tcAlts env scrut alts =
    do t <- fresh
       foldM combine (idSubst, TVar t) alts
  where
    combine (s, t) alt =
        do (s', t') <- tcAlt env scrut alt
           s'' <- unify s' (t, t')
           return (s'' `scomp` s, t')

-- Type-check a single alternative
tcAlt :: TypeEnv -> Type -> Alt -> TI (Subst, Type)
tcAlt env t (PCon name, names, body) =
    do cons <- gets tiCons
       scheme <- case Map.lookup name cons of
           Just x  -> return x
           Nothing -> raise $ "Undeclared constructor " ++ quote name
       t' <- newInstance scheme
       let (types, rtype) = components t'
       s <- unify idSubst (t, rtype)
       when (length types /= length names) $
           raise $ "Wrong number of components for " ++ quote name
       let schemes = map (Scheme []) types
           bindings = zip names schemes
           env'     = Map.fromList bindings `Map.union` env
       (s', t') <- tcExpr env' body
       return (s' `scomp` s, t')
  where
    -- Break a constructor into its components and return type
    components :: Type -> ([Type], Type)
    components = loop [] where
        loop cs (TCon "(->)" [a, b]) = loop (a:cs) b
        loop cs last                  = (reverse cs, last)

{- Dealing with generalization and instantiation -}

-- Generate a mapping from names to new type schemes
toSchemes :: [Name] -> TI TypeEnv
toSchemes ns =
    do s <- withNames ns
       return $ Map.map (Scheme []) s

-- Get the types from a type environment
-- Use name lookup to put them in order
fromSchemes :: TypeEnv -> [Name] -> [Type]
fromSchemes env = foldr find [] where
    find name ts = case Map.lookup name env of
        Just t  -> unScheme t:ts
        Nothing -> ts

-- Get the type from a single type scheme
unScheme :: Scheme -> Type
unScheme (Scheme _ t) = t

-- Get all the types from a mapping of names to type schemes
unSchemeAll :: TypeEnv -> [Type]
unSchemeAll env = map unScheme (Map.elems env)

-- Instantiate a type scheme to a new concrete type
newInstance :: Scheme -> TI Type
newInstance (Scheme vs t) =
    do s <- withNames vs
       return $ apply s t

-- Build a polymorphic version of a monomorphic type
generalize :: TypeEnv -> Type -> TI Scheme
generalize env t =
    do let vs = tvars t `Set.difference` tvars env 
       s <- withNames (Set.toList vs)
       let t'  = apply s t
           vs' = map unTVar (Map.elems s)
       return $ Scheme vs' t'
    where unTVar (TVar n) = n

-- Map each name to a generalized version of its inferred type
addDecls :: TypeEnv -> [Name] -> [Type] -> TI TypeEnv
addDecls env ns ts =
    do schemes <- mapM (generalize env) ts
       return $ Map.fromList (zip ns schemes) `Map.union` env

{- Ensure there are no duplicate bindings in current scope -}

-- Only really interesting case is Let(rec)
findDups :: Expr -> TI Expr
findDups (Let recursive bindings expr) =
    do let (names, exprs) = unzip bindings
       _ <- foldM scan Set.empty names
       exprs' <- mapM findDups exprs
       expr' <- findDups expr
       return $ Let recursive (zip names exprs') expr'
  where
    scan seen name =
        do when (name `Set.member` seen) $
               raise $ "Duplicate binding for " ++ quote name
           return $ Set.insert name seen
findDups (Lambda args e) =
    do e' <- findDups e
       return $ Lambda args e'
findDups (App e1 e2) =
    do e1' <- findDups e1
       e2' <- findDups e2
       return $ App e1' e2'
findDups (Case scrutinee alts) =
    do scrutinee' <- findDups scrutinee
       alts' <- mapM findDupsAlt alts
       return $ Case scrutinee' alts'
  where
    findDupsAlt (PCon name, names, body) =
        do body' <- findDups body
           return $ (PCon name, names, body')
findDups e = return e

{- Build entry point to type inference engine -}

-- Run type-inference and return either an error or some value
runTI :: TI a -> Stage a
runTI ti = evalStateT ti initState
  where
    initState = TIState
        { tiDecl = Nothing
        , tiNext = 0
        , tiTEnv = Map.empty
        , tiCons = Map.fromList
                    [ ("True",  Scheme [] boolTy)
                    , ("False", Scheme [] boolTy)
                    ]
        , tiData = Map.fromList
                    [ ("Int",  intTy)
                    , ("Bool", boolTy)
                    ]
        }

-- Convert a list of declarations into a single letrec and return
-- list of all top-level declarations
-- Each non-nullary binding is turned into a lambda
-- This is also where we check for the existence of main
convertToExpr :: [Declaration] -> TI ([Name], Expr)
convertToExpr combinators =
    do (bindings, main) <- foldM convert ([], Nothing) combinators
       let names = map fst bindings
       case main of
           Just main -> return (names, Let True bindings main)
           Nothing   -> raise "Must specify nullary function main"
  where
    -- Redeclaring main is an error
    convert (xs, Just main) (Combinator "main" []   expr)
        = raise "Redeclaration of main"

    -- Initial declaration of main
    convert (xs, Nothing) (Combinator "main" [] expr)
        = return (("main", expr):xs, Just (Var "main"))

    -- Declaring main with arguments is an error
    convert (xs, Nothing) (Combinator "main" args expr)
        = raise "main must be a nullary declaration"

    -- A binding with no arguments just becomes a regular let-binding
    convert (xs, main) (Combinator name [] expr)
        = return ((name, expr):xs, main)

    -- A binding with arguments becomes a let-bound lambda
    convert (xs, main) (Combinator name args expr)
        = return ((name, Lambda args expr):xs, main)

-- Infer types and return top-level types and untransformed program
inferTypes :: Program -> TI ([(Name, Scheme)], Program)
inferTypes program =
    do -- Break program into data declarations and supercombinators
       let (decls, combinators) = List.partition isData program
           env = Map.fromList primOps

       -- Check data declarations first
       env' <- checkDataTypes env decls

       -- Convert program into a single letrec and find top-level names
       (names, expr) <- convertToExpr combinators

       -- Check for duplicate bindings
       expr' <- findDups expr

       -- Turn flat letrec into a nested let(rec) where each bind-group is
       -- as small as possible
       expr'' <- lift $ simplifyExpr expr'

       -- Type-check expression
       (s, t) <- tcExpr env' expr''

       -- Get inferred types for constructors and top-level names and sort them
       env <- gets tiTEnv
       cons <- gets tiCons
       let topLevel = fromKeys env names `Map.union` cons
           sorted = List.sortBy (compare `on` fst) $ Map.toList topLevel

       -- Return types and (mostly) untransformed program
       return $ (sorted, decls ++ combinators)


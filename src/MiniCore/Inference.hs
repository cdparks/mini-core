{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Inference (
    typecheck       
) where 

import MiniCore.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

-- Public compiler stage
typecheck :: Program -> Stage Program
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

-- Mapping from names to schemes
type TypeEnv = Map.Map Name Scheme

-- Internal type-inference state
data TIState = TIState
    { tiNext :: Int                  -- Generate fresh type variables
    , tiCons :: Map.Map Name Scheme  -- Constructor for case-expressions
    , tiData :: Map.Map Name Type    -- Available types
    }

-- Keep track of type-inference state and be able to throw an error
--type TI a = ErrorT String (StateT TIState Identity) a
--type TI a = ErrorT String (StateT TIState IO) a
type TI a = StateT TIState Stage a

-- Substitute type in for name
type Subst = Map.Map Name Type

-- If something is type-like, we can apply a substitution to it
-- and get a set of its type-variables
class Types a where
    apply :: Subst -> a -> a
    tvars :: a -> Set.Set Name

instance Types Type where
    apply s (TVar n) = case Map.lookup n s of
                         Just t  -> t
                         Nothing -> TVar n
    apply s (TCon n ts) = TCon n $ map (apply s) ts

    tvars (TVar n) = Set.singleton n
    tvars (TCon n ts) = foldr Set.union Set.empty (map tvars ts)

-- Special case; pretty-printTyed types should have the type-variables in order
-- which tvars won't necessarily maintain
tvarsOrdered :: Type -> [Name]
tvarsOrdered (TVar n) = [n]
tvarsOrdered (TCon n ts) = foldr union [] (map tvarsOrdered ts)

-- tvars doesn't return universally quantified type-variables
instance Types Scheme where
    apply s (Scheme vs t) = let s' = foldr Map.delete s vs
                            in Scheme vs (apply s' t)

    tvars (Scheme vs t) = tvars t `Set.difference` Set.fromList vs

-- Extend operations to lists of things that are type-like
instance Types a => Types [a] where
    apply s = map (apply s)
    tvars   = foldr Set.union Set.empty . map tvars

instance Types TypeEnv where
    apply s env = Map.map (apply s) env
    tvars env = tvars (Map.elems env)

-- Compose substitions by applying s1 to s2 and then
-- taking their union
scomp :: Subst -> Subst -> Subst
scomp s1 s2 = let s2' = Map.map (apply s1) s2
              in s2' `Map.union` s1

-- Infinite list of prettier type-variables for printTying
prettyVars :: [String]
prettyVars = alphas ++ alphaNums
  where alphas =
            do char <- ['a'..'z']
               return [char]
        alphaNums =
            do num  <- [1..]
               char <- ['a'..'z']
               return $ char:show num

-- Pretty-print a concrete type
prettyType :: Type -> String
prettyType t = let env = zip (tvarsOrdered t) prettyVars
               in prettyType' env t

-- Pretty-print a universally-quantified type
prettyScheme :: Scheme -> String
prettyScheme (Scheme vars t) =
    let env = zip (nub (vars ++ tvarsOrdered t)) prettyVars
    in prettyForall env vars ++ prettyType' env t

-- Shared code for printing a type with a mapping from the actual
-- type-variables to pretty type-variables
prettyType' :: [(Name, Name)] -> Type -> String
prettyType' env t = loop t where
    loop (TVar n) =
        case lookup n env of
            Just x  -> x
            Nothing -> "?"
    loop (TCon n []) = n
    loop (TCon "(->)" [a, b]) =
        "(" ++ loop a ++ " -> " ++ loop b ++ ")"
    loop (TCon n xs) = "(" ++ n ++ " " ++ intercalate " " (map loop xs) ++ ")"

-- Print universally quantified type variables if there are any
prettyForall :: [(Name, Name)] -> [Name] -> String
prettyForall env [] = ""
prettyForall env xs = "forall " ++ intercalate " " (map get xs) ++ ". "
  where get n = case lookup n env of
                  Just x  -> x
                  Nothing -> "?"

-- Pretty-print all type-schemes in a type-environment
prettyEnv :: TypeEnv -> String
prettyEnv env = intercalate "\n" (map pretty (Map.toList env)) ++ "\n"
  where pretty (name, scheme) = name ++ " :: " ++ prettyScheme scheme

-- Identity substition has no effect when applied
idSubst :: Subst
idSubst = Map.empty

-- If constraint is non-trivial and passes the occurs check,
-- add it to substitution
extend :: Subst -> Name -> Type -> TI Subst
extend s n t
    | t == TVar n            = return s
    | n `Set.member` tvars t = throwError "occurs check failed"
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
    | otherwise = throwError $
                      "Cannot unify " ++ show n ++ " and " ++ show n'

-- Unify each pair of types propagating the substition
unifyAll :: Subst -> [(Type, Type)] -> TI Subst
unifyAll = foldM unify

-- Get a new name
fresh :: TI Name
fresh = do n <- gets tiNext
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

-- Type-check an expression in the current type-environment producing
-- a substitution and a concrete type
tcExpr :: TypeEnv -> Expr -> TI (Subst, Type)

-- Literal integer
tcExpr env (Num _) = return (idSubst, intTy)

-- Variable
tcExpr env (Var x) =
    do scheme <- case Map.lookup x env of
           Nothing     -> throwError $ "Unbound variable: " ++ x
           Just scheme -> return scheme
       t <- newInstance scheme
       return (idSubst, t)

-- Application 
tcExpr env (App e1 e2) =
    do (s, [t1, t2]) <- tcAll env [e1, e2]
       n  <- fresh
       s' <- unify s (t1, t2 `arrow` TVar n)
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
    do let names = map fst bindings
           exprs = map snd bindings
       (s, ts) <- tcAll env exprs
       env'' <- addDecls (apply s env) names ts
       (s', t) <- tcExpr env'' expr
       return (s' `scomp` s, t)

-- Recursive Let
tcExpr env (Let True bindings expr) =
    do let names = map fst bindings
           exprs = map snd bindings
       schemes <- toSchemes names
       (s, ts) <- tcAll (schemes `Map.union` env) exprs
       let schemes' = apply s schemes
           env'     = apply s env
           ts'      = fromSchemes schemes' names
       s' <- unifyAll s (zip ts ts')
       tcBody env' s' schemes' expr

-- Type-check the body of a letrec expression
tcBody :: TypeEnv -> Subst -> TypeEnv -> Expr -> TI (Subst, Type)
tcBody env s schemes e =
    do let ts = unSchemeAll $ apply s schemes
       env' <- addDecls (apply s env) (Map.keys schemes) ts
       (s', t) <- tcExpr env' e
       return (s' `scomp` s, t)

-- Type-check each alternative in a case-expression
tcAlts :: TypeEnv -> Type -> [Alt] -> TI (Subst, Type)
tcAlts env rtype alts = 
    do t <- fresh
       foldM combine (idSubst, TVar t) alts
  where
    combine (s, t) alt =
        do (s', t') <- tcAlt env rtype alt
           s'' <- unify s' (t, t')
           return (s'' `scomp` s, t')

-- Break a constructor into its components and return type
components :: Type -> ([Type], Type)
components = loop [] where
    loop cs (TCon "(->)" [a, b]) = loop (a:cs) b
    loop cs last                  = (reverse cs, last)

-- Type-check a single alternative
tcAlt :: TypeEnv -> Type -> Alt -> TI (Subst, Type)
tcAlt env t (PCon name, names, body) =
    do cons <- gets tiCons
       scheme <- case Map.lookup name cons of
           Just x  -> return x
           Nothing -> throwError $ "Undeclared constructor " ++ name
       t' <- newInstance scheme
       let (types, rtype) = components t'
       s <- unify idSubst (t, rtype)
       when (length types /= length names) $
           throwError $ "Wrong number of components for " ++ name ++ " in case-expression"
       let schemes = map (Scheme []) types
           bindings = zip names schemes
           env'     = Map.fromList bindings `Map.union` env
       (s', t') <- tcExpr env' body
       return (s', t')

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

-- Type check a list of expressions producing a composite substition
-- and a list of types
tcAll :: TypeEnv -> [Expr] -> TI (Subst, [Type])
tcAll env []     = return (idSubst, [])
tcAll env (e:es) =
    do (s, t)   <- tcExpr env e
       (s', ts) <- tcAll (apply s env) es
       return (s' `scomp` s, apply s' t : ts)

-- Run type-inference and return either an error or some value
runTI :: TI a -> Stage a
runTI ti = evalStateT ti initState
  where
    initState = TIState
        { tiNext = 0
        , tiCons = Map.fromList
                    [ ("True",  Scheme [] boolTy)
                    , ("False", Scheme [] boolTy)
                    ]
        , tiData = Map.fromList
                    [ ("Int",  intTy)
                    , ("Bool", boolTy)
                    ]
        }

-- Add a new data type to the tiData mapping
newDataType :: Name -> Type -> TI ()
newDataType name t =
    do types <- gets tiData
       when (name `Map.member` types) $
           throwError $ "Redeclaration of data type " ++ name
       modify $ \s -> s { tiData = Map.insert name t types }

-- Ensure each data declaration is well-formed:
-- * Must not be redeclaration
-- * Each constructor must be well-formed
-- * Type variables are replaced with fresh type variables
checkDataTypes :: TypeEnv -> [Declaration] -> TI TypeEnv
checkDataTypes = foldM checkDataType where
    checkDataType env (Data name vars constructors) =
        do s <- withNames vars
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
               throwError $ "Unbound type variable in constructor " ++ name
           cons <- gets tiCons
           when (name `Map.member` cons) $
               throwError $ "Redeclaration of constructor " ++ name
           mapM_ validType args
           let scheme = mkScheme args
           modify $ \s -> s { tiCons = Map.insert name scheme cons }
           return $ Map.insert name scheme env

    validType (TVar _) = return ()
    validType (TCon n ts) =
        do mapM_ validType ts
           types <- gets tiData
           when (n `Map.notMember` types) $
               throwError $ "Use of undeclared type " ++ n
           return ()

-- Convert a list of declarations into a single letrec
-- Each non-nullary binding is turned into a lambda
-- This is also where we check for the existence of main
convertToExpr :: [Declaration] -> TI Expr
convertToExpr combinators =
    do (bindings, main) <- foldM convert ([], Nothing) combinators
       case (bindings, main) of
           ([],       Just main) -> return main
           (bindings, Just main) -> return $ Let True bindings main
           (_,        Nothing)   -> throwError "Must specify nullary function main"
  where
    -- Redeclaring main is an error
    convert (xs, Just main) (Combinator "main" []   expr)
        = throwError "Redeclaration of main"

    -- Initial declaration of main
    convert (xs, Nothing) (Combinator "main" [] expr)
        = return (("main", expr):xs, Just (Var "main"))

    -- Declaring main with arguments is an error
    convert (xs, Nothing) (Combinator "main" args expr)
        = throwError "main must be a nullary declaration"

    -- A binding with no arguments just becomes a regular let-binding
    convert (xs, main) (Combinator name [] expr)
        = return ((name, expr):xs, main)

    -- A binding with arguments becomes a let-bound lambda
    convert (xs, main) (Combinator name args expr)
        = return ((name, Lambda args expr):xs, main)

-- Infer types and return the untransformed program
inferTypes :: Program -> TI Program
inferTypes program =
    do let (decls, combinators) = partition isData program
           env = Map.fromList primOps
       env' <- checkDataTypes env decls
       expr <- convertToExpr combinators
       (s, t) <- tcExpr env' expr
       --liftIO $ putStrLn $ "main :: " ++ prettyType (apply s t)
       return $ decls ++ combinators


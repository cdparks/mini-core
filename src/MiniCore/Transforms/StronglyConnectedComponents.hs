module MiniCore.Transforms.StronglyConnectedComponents
  ( simplifyProgram
  , simplifyExpr
  ) where

import MiniCore.Types
import MiniCore.Transforms.Utils
import MiniCore.Format

import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Debug.Trace
import Data.Foldable (foldrM)

-- Map vertex to the list of neighbor vertices
type Edges a = Map.Map a [a]

-- Find vertices with an edge from vertex
expand :: Ord a => a -> Edges a -> [a]
expand a edges = maybe [] id (Map.lookup a edges)

-- Use edge map to do depth-first-search from each vertex updating
-- state (visited, sequence). The output sequence should be sorted in
-- topological order
innerDFS :: Ord a => Edges a -> (Set.Set a, [a]) -> [a] -> (Set.Set a, [a])
innerDFS edges = List.foldl' search
 where
  search (visited, sequence) vertex
    | vertex `Set.member` visited = (visited, sequence)
    | otherwise                   = (visited', vertex:sequence')
   where
    (visited', sequence') =
      innerDFS edges (Set.insert vertex visited, sequence) (expand vertex edges)

-- Public interface to depth-first-search
dfs :: Ord a => Edges a -> [a] -> [a]
dfs edges = snd . innerDFS edges (Set.empty, [])

-- Do depth first search from each vertex producing a list of sets
-- of vertices visited.
spanningSearch :: Ord a => Edges a -> [a] -> [Set.Set a]
spanningSearch edges = snd . List.foldl' search (Set.empty, [])
 where
  search (visited, setSequence) vertex
    | vertex `Set.member` visited = (visited, setSequence)
    | otherwise                   = (visited', Set.fromList (vertex:sequence):setSequence)
   where
    (visited', sequence) =
      innerDFS edges (Set.insert vertex visited, []) (expand vertex edges)

-- Construct a topologically sorted sequence of the vertices in the graph
-- and then construct the reverse of the topologically sorted sequence
-- of strongly connected components
scc :: Ord a => Edges a -> Edges a -> [a] -> [Set.Set a]
scc ins outs = spanningSearch ins . dfs outs

-- Annotate program with free-variables and then 
-- break up into the smallest possible lets and letrecs
simplifyProgram :: Program -> Stage Program
simplifyProgram program = depends =<< freeVars program

-- Annotate single expression with free-variables and
-- then break up into the smallest possible let or letrec
simplifyExpr :: Expr -> Stage Expr
simplifyExpr expr = dependsExpr =<< freeVarsExpr Set.empty expr

-- Run dependency analysis on the body of each combinator
depends :: FVProgram -> Stage Program
depends program = mapM depends' program
 where
  depends' (name, args, body) =
    Combinator name args <$> dependsExpr body

-- Lets are the only interesting case
dependsExpr :: FVExpr -> Stage Expr
dependsExpr (free, ANum n) =
  return (Num n)

dependsExpr (free, ACons tag arity) =
  return (Cons tag arity)

dependsExpr (free, AVar v) =
  return (Var v)

dependsExpr (free, AApp e1 e2) =
  App <$> dependsExpr e1 <*> dependsExpr e2

dependsExpr (free, ACase body alts) =
  let dependsAlt (tag, args, e) = (,,) tag args <$> dependsExpr e
  in Case <$> dependsExpr body <*> mapM dependsAlt alts

dependsExpr (free, ALambda args body) =
  Lambda args <$> dependsExpr body

dependsExpr (free, ALet recursive defs body) = do
  let binders = bindersOf defs
      binderSet
        | recursive = Set.fromList binders
        | otherwise = Set.empty

      -- Make an edge from each name to its free variables that are bound
      -- in this letrec
      edges =
        [ (name, freeSet)
        | (name, (freeVars, _)) <- defs
        , freeSet <- Set.toList (freeVars `Set.intersection` binderSet)
        ]

      -- If ins w = [u, ...] then w depends on each u
      -- If out u = [w, ...] then each w depends on u
      ins = Map.fromList [(w, [u | (u, w') <- edges, w == w']) | (_, w) <- edges]
      out = Map.fromList [(u, [w | (u', w) <- edges, u == u']) | (u, _) <- edges]

      -- Strongly connected components in sorted topologically
      components = map Set.toList (scc ins out binders)

      -- Break defs into strongly connected components
      defs' =
        [[(name, fromJust (lookup name defs)) | name <- names]
        | names <- components
        ]

  -- Build new nested let(rec)
  body' <- dependsExpr body
  defs'' <- foldrM mkLet body' defs'
  return defs''

-- Take a list of definitions and build a new Let out of them
-- Make it recursive if any name is found in the set of all
-- free variables
mkLet :: [(Name, FVExpr)] -> Expr -> Stage Expr
mkLet defs body = do
  let names = map fst defs
      exprs = map snd defs
  exprs' <- mapM dependsExpr exprs
  let defs' = zip names exprs'
      vars = foldr Set.union Set.empty (map fst exprs)
      recursive = any (`Set.member` vars) names
  return (Let recursive defs' body)


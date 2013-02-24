-- main = double 21;
-- double x = x + x
--
module Expr where

-- Core Expression type
data Expr = Var Name
          | Num Int
          | Cons Int Int
          | App Expr Expr
          | Let IsRec [(Name, Expr)] Expr
          | Case Expr [Alt]
          | Lambda [Name] Expr

type Name       = String
type IsRec      = Bool
type Alt        = (Int, [Name], Expr)
type Combinator = (Name, [Name], Expr)
type Program    = [Combinator]

-- Need to distinguish between let and recursive let
recursive    = True ::IsRec -- letrec
nonRecursive = False::IsRec -- let

-- Get variable names from environment
bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

-- Get variable values from environment
valuesOf  :: [(a, b)] -> [b]
valuesOf  = map snd

-- Is an expression an atom?
isAtom :: Expr -> Bool
isAtom (Var _) = True
isAtom (Num _) = True
isAtom _       = False

-- Standard library
prelude :: Program
prelude = [
    ("I",  ["x"], Var "x"),
    ("K",  ["x", "y"], Var "x"),
    ("K1", ["x", "y"], Var "y"),
    ("S",  ["f", "g", "x"], App (App (Var "f") (Var "x"))
                                (App (Var "g") (Var "x"))),
    ("compose", ["f", "g", "x"], App (Var "f") (App (Var "g") (Var "x"))),
    ("twice",   ["f"], App (App (Var "compose") (Var "f")) (Var "f"))]


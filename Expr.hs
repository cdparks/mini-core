-- main = double 21;
-- double x = x + x
--
module Expr where

-- Testing formatting
program = [("main", [], (App (Var "double") (Num 21))),
           ("double", ["x"], (Let False [("y", (App (App (Var "+") (Var "x")) (Var "x")))] (Var "y"))),
           ("length", ["ls"], Case (Var "ls") [(0, [], (Num 0)),
                                               (1, ["x", "xs"], (App (App (Var "+") (Num 1)) (App (Var "length") (Var "xs"))))]),
           ("fix", ["f"], Let True [("x", (App (Var "fix") (Var "f")))] (App (Var "f") (Var "x"))),
           ("len", ["ls"], Case (Var "ls") [(0, [], (Num 0)),
                                            (1, ["x", "xs"], Case (Var "xs") [(0, [], (Num 1)),
                                                                              (1, ["y", "ys"], Case (Var "ys") [(0, [], (Num 2)),
                                                                                                                (1, ["z", "zs"], (App (App (Var "+") (Num 3)) (App (Var "len") (Var "zs"))))])])]),
           ("factorial", [], App (Var "fix")
                (Lambda ["f", "n"]
                    (Case (App (App (Var "<") (Var "n")) (Num 2)) [(0, [], (App (App (Var "*") (Var "n")) (App (Var "f") (App (App (Var "-") (Var "n")) (Num 1))))),
                                                                   (1, [], (Num 1))])))]
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


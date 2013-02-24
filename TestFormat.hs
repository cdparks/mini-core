module TestFormat where

import Expr
import Format

-- Test formatting with a few different constructions
program :: Program
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
                                                                   (1, [], (Num 1))]))),
           ("muladd", ["x"],
            (App (App (Var "+")
                 (App (App (Var "*") (Var "x")) (Var "x")))
                 (App (App (Var "*") (Var "x")) (Var "x")))),
           ("appmulad", ["f", "x"],
            (App (App (Var "+")
                 (App (Var "f") (App (App (Var "*") (Var "x")) (Var "x"))))
                 (App (Var "f") (App (App (Var "*") (Var "x")) (Var "x"))))),
           ("addmul", ["x"],
            (App (App (Var "*")
                 (App (App (Var "+") (Var "x")) (Var "x")))
                 (App (App (Var "+") (Var "x")) (Var "x")))),
           ("appaddmul", ["f", "x"],
            (App (App (Var "*")
                 (App (Var "f") (App (App (Var "+") (Var "x")) (Var "x"))))
                 (App (Var "f") (App (App (Var "+") (Var "x")) (Var "x")))))]

main = putStrLn $ showProgram program

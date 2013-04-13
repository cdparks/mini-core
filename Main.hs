module Main where

import Types
import Parse
import Transforms
import GCompiler
import GMachine
import Format

import System.Environment (getArgs)

-- Compile and run program
run :: String -> String
run = show . formatLast . evaluate . compile . transform . parseCore

-- Compile and run program printing intermediate states
debug :: String -> String
debug = show . formatResults . evaluate . compile . transform . parseCore

-- Parse and compile program and evaluate first state.
-- Used for interactive debugging in ghci.
-- example:
-- ghci> start "main = I 10"
--   { lots of output }
-- ghci> next it
--   { lots of output }
-- ghci> next it
--   ...
start :: String -> IO GMState
start p = do
    state <- return (single (compile (transform (parseCore p))))
    putStrLn $ show $ formatResults [state]
    return state

-- Transition to next state
-- Used for interactive debugging in ghci.
next :: GMState -> IO GMState
next state = do
    state' <- return (single state)
    putStrLn $ show $ formatState (state', 0)
    return state'

-- Parse and unparse program
showParse :: String -> String
showParse = show . formatProgram . parseCore

main :: IO ()
main = do
    args <- getArgs
    if length args > 0 then do
        program <- readFile $ head args
        putStrLn $ run $ program
    else
        putStrLn "Must provide source file as first argument"


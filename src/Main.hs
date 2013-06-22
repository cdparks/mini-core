module Main where

import MiniCore.Types
import MiniCore.Parse
import MiniCore.Transforms
import MiniCore.GCompiler
import MiniCore.GMachine
import MiniCore.Format

import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import Data.List

-- Compile and run program
run :: String -> String
run = show . formatLast . evaluate . compile . transform . snd . parseCore

-- Compile and run program printing intermediate states
debug :: String -> String
debug = show . formatResults . evaluate . compile . transform . snd . parseCore

-- Compile program and generate initial state
startStep :: String -> IO ()
startStep program = do
    let state = single $ compile $ transform $ snd $ parseCore program
    putStrLn $ show $ formatFirstStep state
    step state

-- Just print program after transformations
transformed :: String -> String
transformed = show . formatProgram . transform . snd . parseCore

-- Step from one state to the next 
step :: GMState -> IO ()
step state = do
    putStrLn $ show $ formatStep state
    if isFinal state
        then putStrLn $ show $ formatLastStep state
        else ask state

-- Ask if user wants to continue
ask :: GMState -> IO ()
ask state = do
    putStr "Next/Quit? [n/q]:"
    hFlush stdout
    line <- getLine
    case line of
        "q" -> return ()
        "n" -> step $ single state
        _      -> ask state

-- Execution flag
data Flag = Normal
          | Step
          | Verbose
          | Transform
            deriving Show

-- Context is file to execute and execution flag
data Context = Context
    { sFlag :: Flag
    , sFile :: String
    } deriving Show

-- Specify action for each option
options :: [OptDescr (Flag -> IO Flag)]
options = [ Option ['v'] ["verbose"]
                (NoArg $ return . const Verbose)
                "Print each machine state as program executes"
          , Option ['s'] ["step"]
                (NoArg $ return . const Step)
                "Single-step through program execution (n -> next, q -> quit)"
          , Option ['t'] ["transform"]
                (NoArg $ return . const Transform)
                "Just show program after constructor and lambda lifting"
          , Option ['h'] ["help"]
                (NoArg . const $ usage [])
                "Print usage and exit"
          ]

-- Parse options and generate execution context
getContext :: IO Context
getContext = do
    argv <- getArgs
    case getOpt Permute options argv of
        (actions, files, []) -> configure files actions
        (_, _, errors)       -> usage errors

-- Generate context from parsed arguments
configure :: [String] -> [Flag -> IO Flag] -> IO Context
configure files actions = do
    flag <- foldl (>>=) (return Normal) actions
    file <- case files of
        []     -> usage ["Must specify at least one file"]
        file:_ -> return file
    return $ Context { sFlag = flag, sFile = file }

-- Print usage and exit
usage :: [String] -> IO a
usage errors = do
    let header = "Usage: mini-core [OPTION...] file"
    let message = usageInfo header options
    hPutStrLn stderr $ intercalate "\n" [concat errors, message]
    exitWith ExitSuccess

main :: IO ()
main = do
    Context { sFlag = flag, sFile = file } <- getContext
    program <- readFile file
    case flag of
        Normal    -> putStrLn $ run program
        Verbose   -> putStrLn $ debug program
        Transform -> putStrLn $ transformed program
        Step      -> startStep program


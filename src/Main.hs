module Main where

import MiniCore.Types
import MiniCore.Parse
import MiniCore.Inference
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
run :: Flags -> String -> Stage Doc
run flags program = do
    parsed <- parseCore program
    traceStage "Parsing" (traceParse flags) $ format parsed

    (types, checked) <- typecheck parsed
    traceStage "Type Inference" (traceCheck flags) $ format types

    (cons, transformed) <- transform checked
    traceStage "Transforms" (traceTrans flags) $ format transformed

    state <- compile (cons, transformed)
    traceStage "Compilation" (traceComp flags) $ formatDefs state

    state' <- execute (traceExec flags) (traceNext flags) state
    return $ formatStateOutput state'

-- Which stages should print debug information?
data Flags = Flags
    { traceParse :: Bool
    , traceCheck :: Bool
    , traceTrans :: Bool
    , traceComp  :: Bool
    , traceExec  :: Bool
    , traceNext  :: Bool
    } deriving Show

-- By default, just run the program
defaultFlags :: Flags
defaultFlags = Flags
    { traceParse = False
    , traceCheck = False
    , traceTrans = False
    , traceComp  = False
    , traceExec  = False
    , traceNext  = False
    }

-- Context is file to execute and execution flag
data Context = Context
    { sFlags :: Flags
    , sFile  :: String
    } deriving Show

-- Specify action for each option
options :: [OptDescr (Flags -> IO Flags)]
options = [ Option ['h'] ["help"]
                (NoArg . const $ usage [])
                "Print usage and exit"
          , Option [] ["show-parse"]
                (NoArg (\f -> return f { traceParse = True }))
                "Show program after parsing"
          , Option [] ["show-types"]
                (NoArg (\f -> return f { traceCheck = True }))
                "Show types after type-checking"
          , Option [] ["show-simple"]
                (NoArg (\f -> return f { traceTrans = True }))
                "Show program after constructor generation and lambda lifting"
          , Option [] ["show-g-code"]
                (NoArg (\f -> return f { traceComp = True }))
                "Show G-code after compilation"
          , Option [] ["show-states"]
                (NoArg (\f -> return f { traceExec = True }))
                "Dump all machine states"
          , Option [] ["interactive"]
                (NoArg (\f -> return f { traceNext = True }))
                "Print each machine state one at a time as program executes"
          ]

-- Parse options and generate execution context
getContext :: IO Context
getContext = do
    argv <- getArgs
    case getOpt Permute options argv of
        (actions, files, []) -> configure files actions
        (_, _, errors)       -> usage errors

-- Generate context from parsed arguments
configure :: [String] -> [Flags -> IO Flags] -> IO Context
configure files actions = do
    flags <- foldl' (>>=) (return defaultFlags) actions
    file <- case files of
        []     -> usage ["Must specify at least one file"]
        file:_ -> return file
    return $ Context { sFlags = flags, sFile = file }

-- Print usage and exit
usage :: [String] -> IO a
usage errors = do
    let header  = "Usage: mini-core [OPTION...] file"
        message = usageInfo header options
    hPutStrLn stderr $ intercalate "\n" [concat errors, message]
    exitWith ExitSuccess

main :: IO ()
main = do
    Context { sFlags = flags, sFile = file } <- getContext
    program <- readFile file
    runStageIO $ run flags program


--
-- ###################
-- Main program module
-- ###################
--

module Main where

--
-- Imports and auxillary definitions
--

import Data.Map
import System.IO ( getContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexGram
import ParGram
import AbsGram

import Utils
import CompileCheck ( runCompileCheck )
import Interpreter ( runInterpreter )

import ErrM

type ParseFun a = [Token] -> Err a

--
-- Usage function
--

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage ./" ++ progName ++ " <FILENAME>"
    exitFailure

--
-- Program running functions
--

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s = do
    ret <- runErrT $ run' p s
    case ret of
        Bad s -> do
            putStrLn $ "\n\nError: " ++ s
            exitFailure
        _     -> do
            exitSuccess

run' :: ParseFun Program -> String -> ErrIO ()
run' p s = do
    prog <- ErrT $ return $ p $ myLexer s
    runCompileCheck prog
    n <-runInterpreter prog
    liftErrT $ putStrLn $ "\n\nint main() returned value: " ++ show n
--
-- Main
--

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> runFile pProgram f
        _   -> usage
    -- getContents >>= run pProgram

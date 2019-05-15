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
-- Program running functions
--

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
    liftErrT $ putStrLn $ show prog 
    runCompileCheck prog
    n <-runInterpreter prog
    liftErrT $ putStrLn $ "\n\nint main() returned value: " ++ show n
--
-- Main
--

main :: IO ()
main = getContents >>= run pProgram

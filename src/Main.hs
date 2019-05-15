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
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import LexGram
import ParGram
import AbsGram

import Utils
import CompileCheck ( runCompileCheck )
import Interpreter ( runInterpreter )

import ErrM

type ParseFun a = [Token] -> Err a

type Verbosity = Int

--
-- Program running functions
--

run :: ParseFun Program -> String -> IO ()
run p s = do
    ret <- runErrT $ run' p s
    case ret of
        Bad s -> putStrLn $ "Error: " ++ s
        _     -> return ()

run' :: ParseFun Program -> String -> ErrIO ()
run' p s = do
    let ts = myLexer s
    prog <- ErrT $ return $ p ts
    runCompileCheck prog
    n <-runInterpreter prog
    liftErrT $ putStrLn $ "\n\nint main() returned value: " ++ show n
--
-- Main
--

main :: IO ()
main = getContents >>= run pProgram

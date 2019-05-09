--
-- ###################
-- Main program module
-- ###################
--

module Main where

--
-- Imports and auxillary definitions
--

import System.IO ( getContents )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import LexGram
import ParGram
import SkelGram
import PrintGram
import AbsGram

import CompileCheck

import ErrM

type ParseFun a = [Token] -> Err a

--
-- Program running functions
--

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn (f ++ "\n") >> readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
    Bad s -> do
        putStrLn "Parse Failed..."
        putStrLn "Tokens:"
        putStrLn $ show ts
        putStrLn $ show s
        exitFailure
    Ok prog -> do
        putStrLn "Parse Successful!"
        putStrLn $ show prog ++ "\n"
        checkProgram prog
        exitSuccess

--
-- Main
--

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= run pProgram
        fs -> mapM_ (runFile pProgram) fs

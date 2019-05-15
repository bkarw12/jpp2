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
-- Printing the result
--

-- putStrV :: Verbosity -> String -> IO ()
-- putStrV v s = if v >= 1 then putStrLn s else return ()

-- printOutput :: Env -> IO ()
-- printOutput env = mapM_ putStr $ (reverse . output) env

-- printMap :: (Show a, Show b) => Map a b -> IO ()
-- printMap = print . show . toList

-- printEnv :: Verbosity -> Env -> IO ()
-- printEnv v env = do
--     if v >= 1 then do
--         putStrLn "\n\nLocations Environment:"
--         printMap $ lEnv env
--         putStrLn "Variables Environment:"
--         printMap $ vEnv env
--         putStrLn "Functions Environment:"
--         printMap $ fEnv env
--     else return ()
    
-- printRet :: Integer -> IO ()
-- printRet n = putStrLn $ "\n\n\"int main()\" Exit Code: " ++ show n

--
-- Program running functions
--

-- runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
-- runFile v p f = putStrLn ("Filepath: " ++ f ++ "\n") >> readFile f >>= run v p

-- run :: Verbosity -> ParseFun Program -> String -> IO ()
-- run v p s = do 
--     putStr "Parse..."
--     let ts = myLexer s
--     case p ts of
--         Bad s -> do
--             putStrLn "Failed!"
--             putStrV v "Tokens:"
--             putStrV v $ show ts
--             putStrLn $ "Error: " ++ s
--             exitFailure
--         Ok prog -> do
--             putStrLn "Successful!"
--             putStrV v $ show prog ++ "\n"
--             runProgram v prog
--             exitSuccess

-- runProgram :: Verbosity -> Program -> IO ()
-- runProgram v prog = do
--     putStr "Program Check..."
--     case checkProgram prog of
--         Bad e -> do
--             putStrLn "Failed!\n"
--             putStrLn e
--             exitFailure
--         Ok _  -> do
--             putStrLn "Successful!"
--             putStr "Program Execution..."
--             case runInterpreter prog of
--                 Ok (n,env) -> do
--                     putStrLn "Successful!\n"
--                     putStrLn "Output:"
--                     printOutput env
--                     printRet n
--                     printEnv v env
--                 Bad e  -> do
--                     putStrLn "Failed!\n"
--                     putStrLn "Output:"
--                     putStrLn e
--                     exitFailure

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
    runInterpreter prog

--
-- Main
--

main :: IO ()
main = getContents >>= run pProgram

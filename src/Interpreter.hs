--
-- #######################
-- Main Interpreter module
-- #######################
--

module Interpreter where

--
-- Imports
--

import AbsGram
import Errors

import ErrM

--
-- Used data types
--

type Env = ()

--
-- Environment preparation
--

prepareEnv :: Program -> Err Env
prepareEnv prog = return ()

--
-- Main interpreter functions
--

runInterpreter :: Program -> Err ()
runInterpreter prog = do
    env <- prepareEnv prog
    return ()

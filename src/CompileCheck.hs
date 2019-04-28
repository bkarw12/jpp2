module CompileCheck where

import Data.Map
import Control.Monad.State

import AbsGram

import ErrM

type Var = String
type Loc = Int
data Val = VInt Int | VBool Bool | VStr String
type FVal = ([Arg], Block)

type Env = (Map Var Loc, Map Var Type, Map Loc Val, Map Var FVal)

printBad (Bad s) = s

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) (empty, empty, empty, empty) of
    Ok ((),s) -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> StateT Env Err ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> StateT Env Err ()
checkTopDef'' t = state $ \s -> ((),s)

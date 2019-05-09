--
-- #######################
-- Main Interpreter module
-- #######################
--

module Interpreter where

--
-- Imports
--

import Data.Map
import Control.Monad.State
import qualified Data.Bool

import AbsGram
import Errors

import ErrM

--
-- Used data types
--

type Var = String
type Boolean = Data.Bool.Bool
type Loc = Integer
data Val = VInt Integer | VBool Boolean | VStr String | VNone

type LEnv = Map Var Loc
type VEnv = Map Loc (Type, Val)
type FEnv = Map Var ([Var], Block)

data Env = Env {
    lEnv :: LEnv,
    vEnv :: VEnv,
    fEnv :: FEnv
}

type Stt a = StateT Env Err a

--
-- Auxillary functions
--

argToVar :: Arg -> Var
argToVar (Arg _ (Ident var)) = var

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

--
-- Auxillary state functions
--

insertVar :: Type -> Var -> Val -> Stt ()
insertVar t var val = do
    env <- get
    let vs  = vEnv env
        loc = newLoc vs
        ls' = insert var loc $ lEnv env
        vs' = insert loc (t,val) vs
    put env {lEnv = ls', vEnv = vs'}

--
-- Interpreter state functions (main operations on lexemes)
--

interpretExp :: Expr -> Stt Val
interpretExp e = return VNone

--
-- Environment preparation
--

prepareEnv :: Program -> Err Env
prepareEnv p = case runStateT (prepareEnv' p) $ Env empty empty empty of
    Ok (_,s) -> Ok s
    Bad e    -> Bad e

prepareEnv' :: Program -> Stt ()
prepareEnv' (Program topdefs) = mapM_ prepareTopDef topdefs

prepareTopDef :: TopDef -> Stt ()
prepareTopDef (FnDef _ (Ident var) args b) = do
    env <- get
    let fs' = insert var (Prelude.map argToVar args,b) $ fEnv env
    put env {fEnv = fs'}
prepareTopDef (VDef (Decl t items)) = mapM_ (prepareTopDef' t) items

prepareTopDef' :: Type -> Item -> Stt ()
prepareTopDef' t (NoInit (Ident var)) = insertVar t var VNone
prepareTopDef' t (Init (Ident var) e) = do
    val <- interpretExp e
    insertVar t var val

--
-- Main interpreter functions
--

runInterpreter :: Program -> Err ()
runInterpreter prog = do
    env <- prepareEnv prog
    return ()

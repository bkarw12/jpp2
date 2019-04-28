-- Module for all compile-time error checks

module CompileCheck where

-- Imports

import Data.Map
import Control.Monad.State

import AbsGram

import ErrM

-- Used data types

type Var = String
type Loc = Int
data Val = VInt Integer | VBool Bool | VStr String | VNone
type VVal = (Type, Val)
type FVal = (Type, [Arg], Block)

type Env = (Map Var Loc, Map Loc VVal, Map Var FVal)

-- Auxillary functions

newloc :: Map Loc a -> Loc
newloc m 
    | size m == 0 = 0
    | otherwise   = (fst $ findMax m) + 1

-- Raw type checking

tcProg :: Program -> 

-- Checking global declarations

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) (empty, empty, empty) of
    Ok ((),s) -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> StateT Env Err ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> StateT Env Err ()
checkTopDef'' (FnDef t (Ident var) args b) = do
    (ls,vs,fs) <- get
    if notMember var fs then 
        let fs' = insert var (t,args,b) fs 
        in put (ls,vs,fs')
    else lift $ Bad $ "Error: Function redeclaration: " ++ var
checkTopDef'' (VDef (Decl t items)) = mapM_ (checkTopDefV t) items

checkTopDefV :: Type -> Item -> StateT Env Err ()
checkTopDefV t (NoInit (Ident var)) = checkTopDefV' VNone t var
checkTopDefV t (Init (Ident var) e) = do
    val <- calcTopDefVal e
    checkTopDefV' val t var

checkTopDefV' :: Val -> Type -> Var -> StateT Env Err ()
checkTopDefV' val t var = do
    (ls,vs,fs) <- get
    if notMember var ls then
        let loc = newloc vs
            ls' = insert var loc ls
            vs' = insert loc (t,val) vs
        in put (ls',vs',fs)
    else lift $ Bad $ "Error: Global variable redeclaration: " ++ var

calcTopDefVal :: Expr -> StateT Env Err Val -- TODO add more expressions?
calcTopDefVal (ELitInt n)   = lift $ Ok  $ VInt n
calcTopDefVal ELitTrue      = lift $ Ok  $ VBool True
calcTopDefVal ELitFalse     = lift $ Ok  $ VBool False
calcTopDefVal (EString s)   = lift $ Ok  $ VStr s
calcTopDefVal _             = lift $ Bad $ "Error: Expression assigned to global variable is not a constant value."

-- Main error check function

checkProgram :: Program -> IO ()
checkProgram prog = case checkProgram' prog of
    Ok _  -> return ()
    Bad e -> putStrLn e

checkProgram' :: Program -> Err ()
checkProgram' prog = do
    env <- checkTopDef prog
    -- TODO: typeCheck
    return ()

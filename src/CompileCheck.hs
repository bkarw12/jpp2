-- Module for all compile-time error checks

module CompileCheck where

-- Imports

import Data.Map
import Control.Monad.State
import Control.Monad.Reader

import AbsGram

import ErrM

-- Used data types -- TODO: rename some types? (like StateT Env Err)

type Var = String
-- type Loc = Int
data Val = VInt Integer | VBool Bool | VStr String | VNone
type VVal = (Type, Val)
type FVal = (Type, [Arg], Block)

-- type LEnv = Map Var Loc
-- type VEnv = Map Loc VVal
type VEnv = Map Var VVal
type FEnv = Map Var FVal
type Env = (VEnv, FEnv)
-- type Env = (LEnv, VEnv, FEnv)

-- Auxillary functions

-- newloc :: Map Loc a -> Loc
-- newloc m 
--     | Data.Map.null m    = 0
--     | otherwise = (fst $ findMax m) + 1

insertArgs :: Env -> [Arg] -> Env
insertArgs env args = env

-- TODO: export to another file

class TypeCheckable a where
    tc :: a -> StateT Env Err ()

instance TypeCheckable Block where
    tc b = lift $ Bad "Error: Not implemented."

-- instance TypeCheckable TopDef where
--     tc (FnDef t i args b) = lift $ Ok ()
--     tc (VDef d) = lift $ Bad $ "xD"

-- Raw type checking

checkTypeProg :: Env -> Err ()
checkTypeProg env = do
    checkTypeVars env
    checkTypeFuncs env
    return ()

checkTypeVars :: Env -> Err ()
checkTypeVars (venv,_) = mapM_ checkTypeVars' $ toList venv

checkTypeVars' :: (Var, VVal) -> Err ()
checkTypeVars' (var,vval) = case vval of
    (Int, VInt _)   -> Ok ()
    (Bool, VBool _) -> Ok ()
    (Str, VStr _)   -> Ok ()
    _               -> Bad $ "Error: types mismatch for global variable \"" ++ var ++ "\" assignment."

checkTypeFuncs :: Env -> Err ()
checkTypeFuncs env = mapM_ (checkTypeFuncs' env) $ toList fenv
    where (_,fenv) = env

checkTypeFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
checkTypeFuncs' env (_,(_,args,b)) = runStateT (tc b) $ insertArgs env args


-- checkTypes = tcProg

-- tcProg :: Env -> Program -> Err ()
-- tcProg env (Program topdefs) = mapM_ (tcTopDef env) topdefs

-- tcTopDef :: Env -> TopDef -> Err ((), Env) 
-- tcTopDef env t = runStateT (tc t) env

-- Checking global declarations

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) (empty, empty) of
    Ok ((),s) -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> StateT Env Err ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> StateT Env Err ()
checkTopDef'' (FnDef t (Ident var) args b) = do
    (vs,fs) <- get
    if notMember var fs then 
        let fs' = insert var (t,args,b) fs 
        in put (vs,fs')
    else lift $ Bad $ "Error: Function redeclaration: " ++ var
checkTopDef'' (VDef (Decl t items)) = mapM_ (checkTopDefV t) items

checkTopDefV :: Type -> Item -> StateT Env Err ()
checkTopDefV t (NoInit (Ident var)) = checkTopDefV' VNone t var
checkTopDefV t (Init (Ident var) e) = do
    val <- calcTopDefVal e
    checkTopDefV' val t var

checkTopDefV' :: Val -> Type -> Var -> StateT Env Err ()
checkTopDefV' val t var = do
    (vs,fs) <- get
    if notMember var vs then
        let vs' = insert var (t,val) vs
        in put (vs',fs)
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
    checkTypeProg env
    return ()

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
data Val = VInt | VBool | VStr | VNone
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

typeToVal :: Type -> Val
typeToVal Int = VInt
typeToVal Bool = VBool
typeToVal Str = VStr
typeToVal _ = VNone

insertArgs :: Env -> [Arg] -> Env
insertArgs env args = Prelude.foldl insertArg env args

insertArg :: Env -> Arg -> Env
insertArg (venv,fenv) (Arg t (Ident var)) = (insert var (t,typeToVal t) venv,fenv)

-- TODO: export to another file

class TypeCheckable a where
    tc :: a -> StateT Env Err ()

instance TypeCheckable Block where
    tc (Block stmts) = mapM_ tc stmts

instance TypeCheckable Stmt where
    tc stmt = lift $ Bad "Error: Not implemented."

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
    (Int, VInt)   -> Ok ()
    (Bool, VBool) -> Ok ()
    (Str, VStr)   -> Ok ()
    _               -> Bad $ "Error: types mismatch for global variable \"" ++ var ++ "\" assignment."

checkTypeFuncs :: Env -> Err ()
checkTypeFuncs env = mapM_ (checkTypeFuncs' env) $ toList fenv
    where (_,fenv) = env

checkTypeFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
checkTypeFuncs' env (_,(_,args,b)) = runStateT (tc b) $ insertArgs env args

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
calcTopDefVal (ELitInt _)   = lift $ Ok VInt
calcTopDefVal ELitTrue      = lift $ Ok VBool
calcTopDefVal ELitFalse     = lift $ Ok VBool
calcTopDefVal (EString _)   = lift $ Ok VStr
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

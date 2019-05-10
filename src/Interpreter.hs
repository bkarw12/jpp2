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
    deriving (Show)
data RetVal = IRet Val | NoRet
type VVal = (Type, Val)
type FVal = ([Arg], Block)

type LEnv = Map Var Loc
type VEnv = Map Loc VVal
type FEnv = Map Var FVal

-- the interpreter environment
data Env = Env {
    lEnv :: LEnv,       -- location env,            var -> loc
    vEnv :: VEnv,       -- variables state env,     loc -> value
    fEnv :: FEnv        -- functions env,           var -> fvalue
}

type Stt a = StateT Env Err a

--
-- Auxillary functions
--

liftError :: String -> Stt a
liftError s = lift $ Bad $ "Error: " ++ s

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

getLoc :: Var -> Stt Loc
getLoc var = do
    env <- get
    case Data.Map.lookup var $ lEnv env of
        Nothing  -> liftError $ ceVarUndeclared var
        Just loc -> return loc

retvalToVal :: RetVal -> Val
retvalToVal NoRet = VNone
retvalToVal (IRet val) = val

getVVal :: Loc -> Stt VVal
getVVal loc = do
    env <- get
    case Data.Map.lookup loc $ vEnv env of
        Nothing   -> lift $ Bad $ "Unknown error: wrong location?"
        Just vval -> return vval

getVVal' :: Var -> Stt VVal
getVVal' var = do
    loc <- getLoc var
    getVVal loc

getVal :: Var -> Stt Val
getVal var = do
    (_,val) <- getVVal' var
    return val

getFun :: Var -> Stt FVal
getFun var = do
    env <- get
    case Data.Map.lookup var $ fEnv env of
        Nothing   -> lift $ Bad $ "Unknown error: bad function name?"
        Just fval -> return fval

--
-- Auxillary state functions
--

declVar :: Type -> Item -> Stt ()
declVar t (NoInit (Ident var)) = insertVar t var VNone
declVar t (Init (Ident var) e) = do
    val <- interpretExp e
    insertVar t var val

declVars :: Decl -> Stt ()
declVars (Decl t items) = mapM_ (declVar t) items

insertVar :: Type -> Var -> Val -> Stt ()
insertVar t var val = do
    env <- get
    let vs  = vEnv env
        loc = newLoc vs
        ls' = insert var loc $ lEnv env
        vs' = insert loc (t,val) vs
    put env {lEnv = ls', vEnv = vs'}

insertArg :: Arg -> Stt ()
insertArg (Arg t (Ident var)) = insertVar t var VNone

assignVar :: Var -> Val -> Stt ()
assignVar var val = do
    loc <- getLoc var
    (t,_) <- getVVal loc
    env <- get
    let vs' = insert loc (t,val) $ vEnv env
    put env {vEnv = vs'}

exprBool :: Expr -> Stt Boolean
exprBool e = do
    (VBool b) <- interpretExp e
    return b

exprBool2 :: Expr -> Expr -> Stt (Boolean, Boolean)
exprBool2 e1 e2 = do
    b1 <- exprBool e1
    b2 <- exprBool e2
    return (b1,b2)

exprInt :: Expr -> Stt Integer
exprInt e = do
    (VInt n) <- interpretExp e
    return n

exprInt2 :: Expr -> Expr -> Stt (Integer, Integer)
exprInt2 e1 e2 = do
    n1 <- exprInt e1
    n2 <- exprInt e2
    return (n1,n2)

runFunction :: FVal -> Stt Val
runFunction (args,b) = do
    env <- get
    let ls = lEnv env
    mapM_ insertArg args
    ret <- interpretBlock b
    env' <- get
    put env' {lEnv = ls}
    return $ retvalToVal ret

--
-- Interpreter state functions (main operations on lexemes)
--

interpretEnv :: Stt Integer
interpretEnv = do
    env <- get
    fval <- getFun "main"
    (VInt n) <- runFunction fval
    return n

interpretBlock :: Block -> Stt RetVal
interpretBlock (Block stmts) = interpretBlock' stmts

interpretBlock' :: [Stmt] -> Stt RetVal
interpretBlock' [] = return NoRet
interpretBlock' (stmt:stmts) = do
    retval <- interpretStmt stmt
    case retval of
        NoRet -> interpretBlock' stmts
        _     -> return retval

interpretStmt :: Stmt -> Stt RetVal
interpretStmt Empty = return NoRet
interpretStmt (BStmt b) = interpretBlock b
interpretStmt (DeclStmt decl) = do
    declVars decl
    return NoRet
interpretStmt (Ass (Ident var) e) = do
    val <- interpretExp e
    assignVar var val
    return NoRet
interpretStmt (Incr (Ident var)) = do
    (VInt n) <- getVal var
    assignVar var $ VInt $ n + 1
    return NoRet
interpretStmt (Decr (Ident var)) = do
    (VInt n) <- getVal var
    assignVar var $ VInt $ n - 1
    return NoRet
interpretStmt (Ret e) = do
    val <- interpretExp e
    return $ IRet val
interpretStmt VRet = return $ IRet VNone
interpretStmt _ = return NoRet

interpretExp :: Expr -> Stt Val
interpretExp (EVar (Ident var)) = do
    (_,val) <- getVVal' var
    return val
interpretExp (ELitInt n) = return $ VInt n
interpretExp ELitTrue = return $ VBool True
interpretExp ELitFalse = return $ VBool False
interpretExp (EApp (Ident var) es) = return VNone -- TODO
interpretExp (EString s) = return $ VStr s
interpretExp (Neg e) = do
    n <- exprInt e
    return $ VInt n
interpretExp (Not e) = do
    b <- exprBool e
    return $ VBool $ not b
interpretExp (EMul e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        Times -> return $ VInt $ n1 * n2
        Mod   -> return $ VInt $ n1 `mod` n2
        Div   -> do
            if n2 == 0 then liftError $ reDivZero (EMul e1 op e2)
            else return $ VInt $ n1 `div` n2
interpretExp (EAdd e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        Plus  -> return $ VInt $ n1 + n2
        Minus -> return $ VInt $ n1 - n2
interpretExp (ERel e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        LTH -> return $ VBool $ n1 < n2
        LE  -> return $ VBool $ n1 <= n2
        GTH -> return $ VBool $ n1 > n2
        GE  -> return $ VBool $ n1 >= n2 
        EQU -> return $ VBool $ n1 == n2
        NE  -> return $ VBool $ n1 /= n2
interpretExp (EAnd e1 e2) = do
    (b1,b2) <- exprBool2 e1 e2
    return $ VBool $ b1 && b2
interpretExp (EOr e1 e2) = do
    (b1,b2) <- exprBool2 e1 e2
    return $ VBool $ b1 || b2

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
    let fs' = insert var (args,b) $ fEnv env
    put env {fEnv = fs'}
prepareTopDef (VDef decl) = declVars decl

--
-- Main interpreter functions
--

runInterpreter :: Program -> Err (Integer, Env)
runInterpreter prog = do
    env <- prepareEnv prog
    runInterpreter' env

runInterpreter' :: Env -> Err (Integer, Env)
runInterpreter' env = case runStateT (interpretEnv) env of
    Bad e    -> Bad e
    ok       -> ok

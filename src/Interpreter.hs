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
data LoopVal = LNone | LBreak | LCont
type VVal = (Type, Val)
type FVal = ([Arg], Block)

type LEnv = Map Var Loc
type VEnv = Map Loc VVal
type FEnv = Map Var FVal

-- the interpreter environment
data Env = Env {
    lEnv :: LEnv,       -- location env,                var -> loc
    vEnv :: VEnv,       -- variables state env,         loc -> value
    fEnv :: FEnv,       -- functions env,               var -> fvalue
    output :: [String]  -- list of things to stdout     [string]
}

type Stt a = StateT Env Err a

--
-- Predefined functions
--
predefinedFunctions :: [Var]
predefinedFunctions = [
    "printInt",
    "printString"]

--
-- Auxillary functions
--

liftRuntimeError :: String -> Stt a
liftRuntimeError s = do
    env <- get
    lift $ Bad $ (concat . reverse $ output env) ++ "\n\nError: " ++ s

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

getLoc :: Var -> Stt Loc
getLoc var = do
    env <- get
    case Data.Map.lookup var $ lEnv env of
        Nothing  -> liftRuntimeError $ ceVarUndeclared var
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

addOutput :: String -> Stt ()
addOutput s = do
    env <- get
    let out' = s:(output env)
    put env {output = out'}

--
-- Auxillary state functions
--

declVar :: Type -> Item -> Stt ()
declVar t (NoInit (Ident var)) = insertVar t var VNone
declVar t (Init (Ident var) e) = do
    val <- interpretExpr e
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

insertArg :: (Arg, Val) -> Stt ()
insertArg (Arg t (Ident var),val) = insertVar t var val

assignVar :: Var -> Val -> Stt ()
assignVar var val = do
    loc <- getLoc var
    (t,_) <- getVVal loc
    env <- get
    let vs' = insert loc (t,val) $ vEnv env
    put env {vEnv = vs'}

exprBool :: Expr -> Stt Boolean
exprBool e = do
    (VBool b) <- interpretExpr e
    return b

exprBool2 :: Expr -> Expr -> Stt (Boolean, Boolean)
exprBool2 e1 e2 = do
    b1 <- exprBool e1
    b2 <- exprBool e2
    return (b1,b2)

exprInt :: Expr -> Stt Integer
exprInt e = do
    (VInt n) <- interpretExpr e
    return n

exprInt2 :: Expr -> Expr -> Stt (Integer, Integer)
exprInt2 e1 e2 = do
    n1 <- exprInt e1
    n2 <- exprInt e2
    return (n1,n2)

runFunction :: Var -> [Val] -> Stt Val
runFunction var vals = do
    if var `elem` predefinedFunctions then runPredef var vals
    else do
        (args,b) <- getFun var
        env <- get
        let ls = lEnv env
        mapM_ insertArg $ zip args vals
        ret <- interpretBlock b
        env' <- get
        put env' {lEnv = ls}
        return $ retvalToVal ret

runPredef :: Var -> [Val] -> Stt Val
runPredef "printInt" [VInt n] = do
    addOutput $ show n
    return VNone
runPredef "printString" [VStr s] = do
    addOutput s
    return VNone
runPredef _ _ = lift $ Bad "Unknown error: wrong predefined function?"

--
-- Interpreter state functions (main operations on lexemes)
--

interpretEnv :: Stt Integer
interpretEnv = do
    (VInt n) <- runFunction "main" []
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

interpretLoopBlock :: [Stmt] -> Stt (RetVal, LoopVal)
interpretLoopBlock [] = return (NoRet,LNone)
interpretLoopBlock (stmt:stmts) = do
    vals <- interpretLoopStmt stmt
    case vals of
        (NoRet,LNone) -> interpretLoopBlock stmts
        _             -> return vals

interpretLoopStmt :: Stmt -> Stt (RetVal, LoopVal)
interpretLoopStmt Break = return (NoRet,LBreak)
interpretLoopStmt Cont = return (NoRet,LCont)
interpretLoopStmt (BStmt (Block stmts)) = interpretLoopBlock stmts
interpretLoopStmt stmt = do
    retval <- interpretStmt stmt
    return (retval, LNone)

interpretLoop :: Expr -> Stmt -> Stmt -> Stt RetVal
interpretLoop e stmt endStmt = do
    (VBool b) <- interpretExpr e
    if b then do
        vals <- interpretLoopStmt stmt
        case vals of
            (IRet ret,_) -> return $ IRet ret
            (_,LBreak)   -> return NoRet
            _            -> do
                interpretStmt endStmt
                interpretLoop e stmt endStmt
    else return NoRet

interpretStmt :: Stmt -> Stt RetVal
interpretStmt (BStmt b) = interpretBlock b
interpretStmt (DeclStmt decl) = do
    declVars decl
    return NoRet
interpretStmt (Ass (Ident var) e) = do
    val <- interpretExpr e
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
    val <- interpretExpr e
    return $ IRet val
interpretStmt VRet = return $ IRet VNone
interpretStmt (Cond e stmt) = interpretStmt (CondElse e stmt Empty)
interpretStmt (CondElse e stmt1 stmt2) = do
    (VBool b) <- interpretExpr e
    if b then interpretStmt stmt1
    else interpretStmt stmt2
interpretStmt (For (Ident var) e1 e2 stmt) = do
    (n1,n2) <- exprInt2 e1 e2
    assignVar var $ VInt n1
    interpretLoop (ERel (EVar (Ident var)) LE (ELitInt n2)) stmt (Incr (Ident var))
interpretStmt (While e stmt) = interpretLoop e stmt Empty
interpretStmt (SExp e) = do
    interpretExpr e
    return NoRet
interpretStmt _ = return NoRet -- break/continue is interpreted in interpretLoop functions

interpretExpr :: Expr -> Stt Val
interpretExpr (EVar (Ident var)) = do
    (_,val) <- getVVal' var
    return val
interpretExpr (ELitInt n) = return $ VInt n
interpretExpr ELitTrue = return $ VBool True
interpretExpr ELitFalse = return $ VBool False
interpretExpr (EApp (Ident var) es) = do
    vals <- mapM interpretExpr es
    runFunction var vals
interpretExpr (EString s) = return $ VStr s
interpretExpr (Neg e) = do
    n <- exprInt e
    return $ VInt n
interpretExpr (Not e) = do
    b <- exprBool e
    return $ VBool $ not b
interpretExpr (EMul e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        Times -> return $ VInt $ n1 * n2
        Mod   -> do
            if n2 == 0 then liftRuntimeError $ reModZero (EMul e1 op e2)
            else return $ VInt $ n1 `mod` n2
        Div   -> do
            if n2 == 0 then liftRuntimeError $ reDivZero (EMul e1 op e2)
            else return $ VInt $ n1 `div` n2
interpretExpr (EAdd e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        Plus  -> return $ VInt $ n1 + n2
        Minus -> return $ VInt $ n1 - n2
interpretExpr (ERel e1 op e2) = do
    (n1,n2) <- exprInt2 e1 e2
    case op of
        LTH -> return $ VBool $ n1 < n2
        LE  -> return $ VBool $ n1 <= n2
        GTH -> return $ VBool $ n1 > n2
        GE  -> return $ VBool $ n1 >= n2 
        EQU -> return $ VBool $ n1 == n2
        NE  -> return $ VBool $ n1 /= n2
interpretExpr (EAnd e1 e2) = do
    (b1,b2) <- exprBool2 e1 e2
    return $ VBool $ b1 && b2
interpretExpr (EOr e1 e2) = do
    (b1,b2) <- exprBool2 e1 e2
    return $ VBool $ b1 || b2

--
-- Environment preparation
--

prepareEnv :: Program -> Err Env
prepareEnv p = case runStateT (prepareEnv' p) $ Env empty empty empty [] of
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

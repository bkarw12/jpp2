-- 
-- ########################################
-- Module for all compile-time error checks
-- ########################################
--

module CompileCheck where

-- 
-- Imports
--

import Data.Map
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Bool

import AbsGram
import Errors
import Utils

import ErrM

--
-- Used data types
--

data Val = VInt | VBool | VStr | VNone
    deriving (Eq)
type VVal = (Type, Val, Integer, Boolean)
type FVal = (Type, [Arg], Block)

type LEnv = Map Var Loc
type VEnv = Map Loc VVal
type FEnv = Map Var FVal

-- the environment created to manage type checks
data Env = Env {
    lEnv :: LEnv,       -- location env,            var -> loc
    vEnv :: VEnv,       -- variables state env,     loc -> value
    fEnv :: FEnv,       -- functions env,           var -> fvalue
    depth :: Integer,   -- variable decl. env,      int,            used to check for redeclarations in one block
    ret :: Type         -- expected return type,    type,           used to check if the return expr type matches
}

type Stt a = StateT Env Err a

--
-- Predefined functions
--

predefinedFunctions :: FEnv
predefinedFunctions = fromList [
    ("printInt",(Void,[Arg Int (Ident "")],Block [])),
    ("printString",(Void,[Arg Str (Ident "")],Block []))]

--
-- Auxillary functions
--

liftCompiletimeError :: String -> Stt a
liftCompiletimeError s = lift $ Bad $ "Error: " ++ s

typeToVal :: Type -> Val
typeToVal Int = VInt
typeToVal Bool = VBool
typeToVal Str = VStr
typeToVal _ = VNone

--
-- Auxillary state functions
--

getLoc :: Var -> Stt Loc
getLoc var = do
    env <- get
    case Data.Map.lookup var $ lEnv env of
        Nothing  -> liftCompiletimeError $ ceVarUndeclared var
        Just loc -> return loc

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

initVar :: Var -> Stt ()
initVar var = do
    env <- get
    loc <- getLoc var
    (t,_,num,ro) <- getVVal loc
    let vs' = insert loc (t,typeToVal t,num,ro) $ vEnv env
    put env {vEnv = vs'}

checkInit :: Expr -> Stt ()
checkInit (EVar (Ident var)) = do
    (_,val,_,_) <- getVVal' var
    if val == VNone then liftCompiletimeError $ ceVarNoInit var
    else return ()
checkInit (Neg e) = checkInit e
checkInit (Not e) = checkInit e
checkInit (EMul e1 _ e2) = do 
    checkInit e1 
    checkInit e2
checkInit (EAdd e1 _ e2) = checkInit (EMul e1 Times e2)
checkInit (ERel e1 _ e2) = checkInit (EMul e1 Times e2)
checkInit (EAnd e1 e2) = checkInit (EMul e1 Times e2)
checkInit (EOr e1 e2) = checkInit (EMul e1 Times e2)
checkInit _ = return ()

insertVar :: Var -> VVal -> Stt ()
insertVar var vval = do
    env <- get
    let vs = vEnv env
        loc = newLoc vs
        ls' = insert var loc $ lEnv env
        vs' = insert loc vval vs
    put env {lEnv = ls', vEnv = vs'}

insertArg :: Arg -> Stt ()
insertArg (Arg t (Ident var)) = do
    env <- get
    insertVar var (t,typeToVal t,depth env,False)

checkInsertVar :: Type -> Item -> Stt ()
checkInsertVar Void _ = liftCompiletimeError ceVarVoid
checkInsertVar (Fun _ _) _ = liftCompiletimeError ceVarFun
checkInsertVar t (NoInit (Ident var)) = checkInsertVar' t VNone var
checkInsertVar t (Init (Ident var) e) = do
    te <- tcExpr e
    checkInit e
    if t == te then checkInsertVar' t (typeToVal t) var
    else liftCompiletimeError $ ceVarDeclType var

checkInsertVar' :: Type -> Val -> Var -> Stt ()
checkInsertVar' t val var = do
    checkReadOnly var $ ceVarRedeclRead var
    env <- get
    let n = depth env
    case Data.Map.lookup var $ lEnv env of
        Nothing  -> return ()
        Just oldLoc -> do
            (_,_,m,_) <- getVVal oldLoc
            if n <= m then liftCompiletimeError $ ceVarRedecl var
            else return ()
    insertVar var (t,val,n,False)

checkArgs :: [Arg] -> [Expr] -> String -> String -> Stt ()
checkArgs args es s1 s2
    | length args /= length es  = liftCompiletimeError s1
    | otherwise                 = mapM_ (checkArgs' s2) $ zip args es

checkArgs' :: String -> (Arg, Expr) -> Stt ()
checkArgs' s ((Arg t _), e) = tcExpr' t e s

setReadOnly :: Boolean -> Var -> Stt ()
setReadOnly ro var = do
    (t,val,num,_) <- getVVal' var
    insertVar var (t,val,num,ro)

checkReadOnly :: Var -> String -> Stt ()
checkReadOnly var s = do
    (_,_,_,ro) <- getVVal' var
    if ro then liftCompiletimeError s
    else return ()

--
-- Raw type checking
--

tcProg :: Env -> Err ()
tcProg env = do
    tcVars env
    tcFuncs env
    return ()

tcVars :: Env -> Err ()
tcVars env = mapM_ (tcVars' $ lEnv env) $ toList $ vEnv env

tcVars' :: LEnv -> (Loc, VVal) -> Err ()
tcVars' ls (loc,vval) = case vval of
    (Int, VInt,_,_)   -> Ok ()
    (Bool, VBool,_,_) -> Ok ()
    (Str, VStr,_,_)   -> Ok ()
    _               -> Bad $ ceVarGlobalType $ keyFromValue ls loc

tcFuncs :: Env -> Err ()
tcFuncs env = mapM_ (tcFuncs' env) $ toList $ fEnv env

tcFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
tcFuncs' env (_,(t,args,b)) = runStateT (tcFunc b args) $ env {ret = t}

tcFunc :: Block -> [Arg] -> Stt ()
tcFunc b args = do
    mapM_ insertArg args
    tcBlock b

tcBlock :: Block -> Stt ()
tcBlock (Block stmts) = mapM_ tcStmt stmts

tcStmt :: Stmt -> Stt ()
tcStmt (BStmt b) = do
    env <- get
    put env {depth = depth env + 1}
    tcBlock b
    env' <- get
    put env {vEnv = vEnv env', fEnv = fEnv env'}
tcStmt (DeclStmt (Decl t items)) = mapM_ (checkInsertVar t) items
tcStmt (Ass id e) = do
    t <- tcExpr e
    checkInit e
    tcExpr'NoInit t (EVar id) $ ceTypeAss (Ass id e)
    let (Ident var) = id
    checkReadOnly var $ ceVarReadOnly var
    initVar var
tcStmt (Incr id) = tcExpr' Int (EVar id) $ ceTypeInc id
tcStmt (Decr id) = tcExpr' Int (EVar id) $ ceTypeDec id
tcStmt (Cond e stmt) = do 
    tcExpr' Bool e $ ceTypeCond e
    tcStmt stmt
tcStmt (CondElse e stmt1 stmt2) = do
    tcStmt (Cond e stmt1)
    tcStmt stmt2
tcStmt (For id e1 e2 stmt) = do
    tcExpr'NoInit Int (EVar id) $ ceTypeFor id
    tcExpr' Int e1 $ ceTypeFor1 e1
    tcExpr' Int e2 $ ceTypeFor2 e2
    let (Ident var) = id
    initVar var
    setReadOnly True var
    tcStmt stmt
    setReadOnly False var
tcStmt (While e stmt) = do
    tcExpr' Bool e $ ceTypeWhile e
    tcStmt stmt
tcStmt (SExp e) = do 
    tcExpr e
    return ()
tcStmt (Ret e) = do
    env <- get
    tcExpr' (ret env) e $ ceTypeRet (Ret e)
tcStmt VRet = do
    env <- get
    if ret env == Void then return ()
    else liftCompiletimeError $ ceTypeRet VRet
tcStmt _ = return ()

tcExpr :: Expr -> Stt Type
tcExpr (EVar (Ident var)) = do
    (t,_,_,_) <- getVVal' var
    return t
tcExpr (ELitInt _) = return Int
tcExpr ELitTrue = return Bool
tcExpr ELitFalse = return Bool
tcExpr (EApp (Ident var) es) = do
    env <- get
    case Data.Map.lookup var $ fEnv env of
        Nothing         -> liftCompiletimeError $ ceFunUndeclared var
        Just (t,args,_) -> do
            checkArgs args es 
                (ceFunPars (EApp (Ident var) es))
                (ceFunType (EApp (Ident var) es))
            return t
tcExpr (EString _) = return Str
tcExpr (Neg e) = do
    tcExpr' Int e $ ceTypeNegInt e
    return Int
tcExpr (Not e) = do
    tcExpr' Bool e $ ceTypeNotBool e
    return Bool
tcExpr (EMul e1 _ e2) = do
    tcExpr2' Int e1 e2 
        (ceTypeInt e1) 
        (ceTypeInt e2) 
    return Int
tcExpr (EAdd e1 _ e2) = tcExpr (EMul e1 Times e2)
tcExpr (ERel e1 _ e2) = do
    tcExpr (EMul e1 Times e2)
    return Bool
tcExpr (EAnd e1 e2) = do
    tcExpr2' Bool e1 e2
        (ceTypeBool e1)
        (ceTypeBool e2)
    return Bool
tcExpr (EOr e1 e2) = tcExpr (EAnd e1 e2)

tcExpr' :: Type -> Expr -> String -> Stt ()
tcExpr' t e s = do
    te <- tcExpr e
    checkInit e
    if t == te then return ()
    else liftCompiletimeError s

tcExpr'NoInit :: Type -> Expr -> String -> Stt ()
tcExpr'NoInit t e s = do
    te <- tcExpr e
    if t == te then return ()
    else liftCompiletimeError s

tcExpr2' :: Type -> Expr -> Expr -> String -> String -> Stt ()
tcExpr2' t e1 e2 s1 s2 = do
    tcExpr' t e1 s1
    tcExpr' t e2 s2

--
-- Checking global declarations
--

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) $ Env empty empty predefinedFunctions 1 Void of
    Ok (_,s)  -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> Stt ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> Stt ()
checkTopDef'' (FnDef t (Ident var) args b) = do
    env <- get
    let fs = fEnv env
    if notMember var fs then 
        let fs' = insert var (t,args,b) fs 
        in put env {fEnv = fs'}
    else liftCompiletimeError $ ceFunRedecl var
checkTopDef'' (VDef (Decl t items)) = mapM_ (checkTopDefV t) items

checkTopDefV :: Type -> Item -> Stt ()
checkTopDefV t (NoInit (Ident var)) = checkTopDefV' VNone t var
checkTopDefV t (Init (Ident var) e) = do
    val <- calcTopDefVal e
    checkTopDefV' val t var

checkTopDefV' :: Val -> Type -> Var -> Stt ()
checkTopDefV' val t var = do
    env <- get
    let ls = lEnv env
    if notMember var ls then insertVar var (t,val,0,False)
    else liftCompiletimeError $ ceVarGlobalRedecl var

calcTopDefVal :: Expr -> Stt Val
calcTopDefVal (ELitInt _) = lift $ Ok VInt
calcTopDefVal ELitTrue = lift $ Ok VBool
calcTopDefVal ELitFalse = lift $ Ok VBool
calcTopDefVal (EString _) = lift $ Ok VStr
calcTopDefVal (Neg e) = calcTopDefVal' e VInt $ ceTypeNegInt e
calcTopDefVal (Not e) = calcTopDefVal' e VBool $ ceTypeNotBool e
calcTopDefVal (EMul e1 _ e2) = calcTopDefVal2' e1 e2 VInt
    (ceTypeInt e1)
    (ceTypeInt e2)
calcTopDefVal (EAdd e1 _ e2) = calcTopDefVal (EMul e1 Times e2)
calcTopDefVal (ERel e1 _ e2) = do
    calcTopDefVal (EMul e1 Times e2)
    return VBool
calcTopDefVal (EAnd e1 e2) = calcTopDefVal2' e1 e2 VBool
    (ceTypeBool e1)
    (ceTypeBool e2)
calcTopDefVal (EOr e1 e2) = calcTopDefVal (EAnd e1 e2)
calcTopDefVal _  = liftCompiletimeError ceVarGlobalAss

calcTopDefVal' :: Expr -> Val -> String -> Stt Val
calcTopDefVal' e v s = do
    val <- calcTopDefVal e
    if val /= v then liftCompiletimeError s
    else return v

calcTopDefVal2' :: Expr -> Expr -> Val -> String -> String -> Stt Val
calcTopDefVal2' e1 e2 v s1 s2 = do
    calcTopDefVal' e1 v s1
    calcTopDefVal' e2 v s2

--
-- Main function check
--

checkMain :: Env -> Err ()
checkMain env = case Data.Map.lookup "main" $ fEnv env of
    Nothing -> Bad $ ceMainNotFound
    Just (t,args,_) -> if t /= Int then Bad $ ceMainRet
        else if args /= [] then Bad $ ceMainArgs
        else return ()

--
-- Check function returns (excl. void functions)
--

checkReturn :: Env -> Err ()
checkReturn env = mapM_ checkReturn' $ toList $ fEnv env

-- we don't need to check for return in void functions (it is caught in types mismatch)
checkReturn' :: (Var, FVal) -> Err ()
checkReturn' (_,(Void,_,_)) = return () 
checkReturn' (var,(_,_,b)) = case checkReturnBlock b of
    Ok _  -> Bad $ ceNonVoidEnd var
    Bad _ -> return ()

checkReturnBlock :: Block -> Err ()
checkReturnBlock (Block stmts) = mapM_ checkReturnStmt stmts

checkReturnStmt :: Stmt -> Err ()
checkReturnStmt (Ret _) = Bad "Ok"
checkReturnStmt (VRet)  = Bad "Ok"
checkReturnStmt _       = return ()

--
-- Main error check function
--

checkProgram :: Program -> Err ()
checkProgram prog = do
    env <- checkTopDef prog
    tcProg env
    checkReturn env
    checkMain env
    return ()

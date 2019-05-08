-- Module for all compile-time error checks

module CompileCheck where

-- Imports

import Data.Map
import qualified Data.Set
import Control.Monad.State
import Control.Monad.Reader

import AbsGram
import PrintGram

import ErrM

-- Used data types -- TODO: rename some types? (like StateT Env Err)

type Set = Data.Set.Set

type Var = String
type Loc = Int
data Val = VInt | VBool | VStr | VNone
    deriving (Eq)
type VVal = (Type, Val, Integer)
type FVal = (Type, [Arg], Block)

type LEnv = Map Var Loc
type VEnv = Map Loc VVal
type FEnv = Map Var FVal
type REnv = Set Var
-- type Env = (LEnv, VEnv, FEnv, REnv, Integer, Type)
data Env = Env {
    lEnv :: LEnv,
    vEnv :: VEnv,
    fEnv :: FEnv,
    rEnv :: REnv,
    depth :: Integer,
    ret :: Type
}

-- Auxillary functions

liftError :: Print a => a -> String -> StateT Env Err ()
liftError x s = return ()

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

-- TODO: generalize these getters?

getLoc :: Var -> StateT Env Err (Loc)
getLoc var = do
    env <- get
    case Data.Map.lookup var $ lEnv env of
        Nothing  -> lift $ Bad $ "Error: variable " ++ show var ++ " not declared."
        Just loc -> return loc

getVVal :: Loc -> StateT Env Err (VVal)
getVVal loc = do
    env <- get
    case Data.Map.lookup loc $ vEnv env of
        Nothing   -> lift $ Bad $ "Unknown error: wrong location?"
        Just vval -> return vval

getVVal' :: Var -> StateT Env Err (VVal)
getVVal' var = do
    loc <- getLoc var
    getVVal loc

keyFromValue :: Eq b => Map a b -> b -> a
keyFromValue m val = fst $ head $ toList $ Data.Map.filter (== val) m

typeToVal :: Type -> Val
typeToVal Int = VInt
typeToVal Bool = VBool
typeToVal Str = VStr
typeToVal _ = VNone

insertArgs :: Env -> [Arg] -> Env
insertArgs env args = Prelude.foldl insertArg env args

insertArg :: Env -> Arg -> Env
insertArg env (Arg t (Ident var)) = env {lEnv = ls', vEnv = vs'}
    where   vs  = vEnv env
            loc = newLoc vs
            ls' = insert var loc $ lEnv env
            vs' = insert loc (t,typeToVal t,depth env) vs

checkInsertVar :: Type -> Item -> StateT Env Err ()
checkInsertVar Void _ = lift $ Bad $ "Error: Variable declared with wrong type: void."
checkInsertVar (Fun _ _) _ = lift $ Bad $ "Error: Variable declared with wrong type: fun."
checkInsertVar t (NoInit (Ident var)) = checkInsertVar' t VNone var
checkInsertVar t (Init (Ident var) e) = do
    te <- tcExpr e
    checkInit e
    if t == te then checkInsertVar' t (typeToVal t) var
    else lift $ Bad $ "Error: Types mismatch in variable declaration: \"" ++ var ++ "\"."

checkInsertVar' :: Type -> Val -> Var -> StateT Env Err ()
checkInsertVar' t val var = do
    checkReadOnly var $ "Cannot redeclare read-only variable " ++ var ++ "."
    env <- get
    let vs  = vEnv env
        ls  = lEnv env
        n   = depth env
        loc = newLoc vs
    case Data.Map.lookup var ls of
        Nothing  -> return ()
        Just oldLoc -> do
            (_,_,m) <- getVVal oldLoc
            if n <= m then lift $ Bad $ "Error: Variable redeclaration: " ++ var ++ "."
            else return ()
    let ls' = insert var loc ls
        vs' = insert loc (t,val,n) vs
    put env {lEnv = ls', vEnv = vs'}

checkArgs :: [Arg] -> [Expr] -> String -> String -> StateT Env Err ()
checkArgs args es s1 s2
    | length args /= length es  = lift $ Bad s1
    | otherwise                 = mapM_ (checkArgs' s2) $ zip args es

checkArgs' :: String -> (Arg, Expr) -> StateT Env Err ()
checkArgs' s ((Arg t _), e) = tcExpr' t e s

checkInit :: Expr -> StateT Env Err ()
checkInit (EVar (Ident var)) = do
    (_,val,_) <- getVVal' var
    if val == VNone then lift $ Bad $ "Error: variable " ++ show var ++ " not initialized."
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

initVar :: Var -> StateT Env Err ()
initVar var = do
    env <- get
    loc <- getLoc var
    (t,_,num) <- getVVal loc
    let vs' = insert loc (t,typeToVal t,num) $ vEnv env
    put env {vEnv = vs'}

checkReadOnly :: Var -> String -> StateT Env Err ()
checkReadOnly var s = do
    env <- get
    if Data.Set.member var $ rEnv env then lift $ Bad s
    else return ()
        
-- Raw type checking

tcBlock :: Block -> StateT Env Err ()
tcBlock (Block stmts) = mapM_ tcStmt stmts

tcStmt :: Stmt -> StateT Env Err ()
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
    tcExpr'NoInit t (EVar id) $ "Error: Types mismatch in variable assignment: \"" ++ show id ++ "\"."
    let (Ident var) = id
    checkReadOnly var $ "Error: cannot assign value to read-only variable: " ++ var ++ "."
    initVar var
tcStmt (Incr id) = tcExpr' Int (EVar id) $ "Error: Types mismatch in variable incrementation: \"" ++ show id ++ "\"."
tcStmt (Decr id) = tcExpr' Int (EVar id) $ "Error: Types mismatch in variable decrementation: \"" ++ show id ++ "\"."
tcStmt (Cond e stmt) = do 
    tcExpr' Bool e $ "Error: Condition expression " ++ show e ++ " is not a boolean."
    tcStmt stmt
tcStmt (CondElse e stmt1 stmt2) = do
    tcStmt (Cond e stmt1)
    tcStmt stmt2
tcStmt (For id e1 e2 stmt) = do
    tcExpr'NoInit Int (EVar id) $ "Error: For loop argument " ++ show id ++ " is not an int."
    tcExpr' Int e1 $ "Error: For loop start expression " ++ show e1 ++ " is not an int."
    tcExpr' Int e2 $ "Error: For loop end expression " ++ show e1 ++ " is not an int."
    let (Ident var) = id
    initVar var
    env <- get
    let rs' = Data.Set.insert var $ rEnv env
    put env {rEnv = rs'}
    tcStmt stmt
tcStmt (While e stmt) = do
    tcExpr' Bool e $ "Error: While loop condition expression " ++ show e ++ " is not a boolean."
    tcStmt stmt
tcStmt (SExp e) = do 
    tcExpr e
    return ()
tcStmt (Ret e) = do
    env <- get
    tcExpr' (ret env) e $ "Error: Types mismatch in return: " ++ show (Ret e) ++ "."
tcStmt VRet = do
    env <- get
    if ret env == Void then return ()
    else lift $ Bad $ "Error: Types mismatch in return: " ++ show VRet ++ "."
tcStmt _ = return ()

tcExpr :: Expr -> StateT Env Err Type
tcExpr (EVar (Ident var)) = do
    (t,_,_) <- getVVal' var
    return t
tcExpr (ELitInt _) = return Int
tcExpr ELitTrue = return Bool
tcExpr ELitFalse = return Bool
tcExpr (EApp (Ident var) es) = do
    env <- get
    case Data.Map.lookup var $ fEnv env of
        Nothing         -> lift $ Bad $ "Error: function \"" ++ show var ++ "\" not declared."
        Just (t,args,_) -> do
            checkArgs args es 
                ("Error: Wrong number of parameters in function statement: " ++ show (EApp (Ident var) es) ++ ".")
                ("Error: Types mismatch in function statement: " ++ show (EApp (Ident var) es) ++ ".")
            return t
tcExpr (EString _) = return Str
tcExpr (Neg e) = do
    tcExpr' Int e $ "Error: Negated expression " ++ show e ++ " not an int."
    return Int
tcExpr (Not e) = do
    tcExpr' Bool e $ "Error: \"Not\" expression " ++ show e ++ " not a boolean."
    return Bool
tcExpr (EMul e1 _ e2) = do
    tcExpr2' Int e1 e2 
        ("Error: expression " ++ show e1 ++ " not an int.") 
        ("Error: expression " ++ show e2 ++ " not an int.")
    return Int
tcExpr (EAdd e1 _ e2) = tcExpr (EMul e1 Times e2)
tcExpr (ERel e1 _ e2) = do
    tcExpr (EMul e1 Times e2)
    return Bool
tcExpr (EAnd e1 e2) = do
    tcExpr2' Bool e1 e2
        ("Error: expression " ++ show e1 ++ " not a boolean.")
        ("Error: expression " ++ show e2 ++ " not a boolean.")
    return Bool
tcExpr (EOr e1 e2) = tcExpr (EAnd e1 e2)

tcExpr' :: Type -> Expr -> String -> StateT Env Err ()
tcExpr' t e s = do
    te <- tcExpr e
    checkInit e
    if t == te then return ()
    else lift $ Bad s

tcExpr'NoInit :: Type -> Expr -> String -> StateT Env Err ()
tcExpr'NoInit t e s = do
    te <- tcExpr e
    if t == te then return ()
    else lift $ Bad s

tcExpr2' :: Type -> Expr -> Expr -> String -> String -> StateT Env Err ()
tcExpr2' t e1 e2 s1 s2 = do
    tcExpr' t e1 s1
    tcExpr' t e2 s2

tcProg :: Env -> Err ()
tcProg env = do
    tcVars env
    tcFuncs env
    return ()

tcVars :: Env -> Err ()
tcVars env = mapM_ (tcVars' $ lEnv env) $ toList $ vEnv env

tcVars' :: LEnv -> (Loc, VVal) -> Err ()
tcVars' ls (loc,vval) = case vval of
    (Int, VInt,_)   -> Ok ()
    (Bool, VBool,_) -> Ok ()
    (Str, VStr,_)   -> Ok ()
    _               -> Bad $ "Error: types mismatch for global variable \"" ++ (keyFromValue ls loc) ++ "\" assignment."

tcFuncs :: Env -> Err ()
tcFuncs env = mapM_ (tcFuncs' env) $ toList $ fEnv env

tcFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
tcFuncs' env (_,(t,args,b)) = runStateT (tcBlock b) $ insertArgs env' args
    where env' = env {ret = t}

-- Checking global declarations

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) $ Env empty empty empty Data.Set.empty 1 Void of
    Ok (_,s) -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> StateT Env Err ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> StateT Env Err ()
checkTopDef'' (FnDef t (Ident var) args b) = do
    env <- get
    let fs = fEnv env
    if notMember var fs then 
        let fs' = insert var (t,args,b) fs 
        in put env {fEnv = fs'}
    else lift $ Bad $ "Error: Function redeclaration: " ++ var ++ "."
checkTopDef'' (VDef (Decl t items)) = mapM_ (checkTopDefV t) items

checkTopDefV :: Type -> Item -> StateT Env Err ()
checkTopDefV t (NoInit (Ident var)) = checkTopDefV' VNone t var
checkTopDefV t (Init (Ident var) e) = do
    val <- calcTopDefVal e
    checkTopDefV' val t var

checkTopDefV' :: Val -> Type -> Var -> StateT Env Err ()
checkTopDefV' val t var = do
    env <- get
    let ls = lEnv env
    if notMember var ls then
        let vs  = vEnv env
            loc = newLoc vs
            ls' = insert var loc ls
            vs' = insert loc (t,val,0) vs
        in put env {lEnv = ls', vEnv = vs'}
    else lift $ Bad $ "Error: Global variable redeclaration: " ++ var ++ "."

calcTopDefVal :: Expr -> StateT Env Err Val -- TODO add more expressions?
calcTopDefVal (ELitInt _) = lift $ Ok VInt
calcTopDefVal ELitTrue = lift $ Ok VBool
calcTopDefVal ELitFalse = lift $ Ok VBool
calcTopDefVal (EString _) = lift $ Ok VStr
calcTopDefVal (Neg e) = calcTopDefVal' e VInt $ "Error: Negated expression " ++ show e ++ " not an int."
calcTopDefVal (Not e) = calcTopDefVal' e VBool $ "Error: \"Not\" expression " ++ show e ++ " not a boolean."
calcTopDefVal (EMul e1 _ e2) = calcTopDefVal2' e1 e2 VInt
    ("Error: Expression " ++ show e1 ++ " not an int.")
    ("Error: Expression " ++ show e2 ++ " not an int.")
calcTopDefVal (EAdd e1 _ e2) = calcTopDefVal (EMul e1 Times e2)
calcTopDefVal (ERel e1 _ e2) = do
    calcTopDefVal (EMul e1 Times e2)
    return VBool
calcTopDefVal (EAnd e1 e2) = calcTopDefVal2' e1 e2 VBool
    ("Error: Expression " ++ show e1 ++ " not a boolean.")
    ("Error: Expression " ++ show e2 ++ " not a boolean.")
calcTopDefVal (EOr e1 e2) = calcTopDefVal (EAnd e1 e2)
calcTopDefVal _  = lift $ Bad $ "Error: Expression assigned to global variable is not a constant value."

calcTopDefVal' :: Expr -> Val -> String -> StateT Env Err Val
calcTopDefVal' e v s = do
    val <- calcTopDefVal e
    if val /= v then lift $ Bad s
    else return v

calcTopDefVal2' :: Expr -> Expr -> Val -> String -> String -> StateT Env Err Val
calcTopDefVal2' e1 e2 v s1 s2 = do
    calcTopDefVal' e1 v s1
    calcTopDefVal' e2 v s2

-- Main function check

checkMain :: Env -> Err ()
checkMain env = case Data.Map.lookup "main" $ fEnv env of
    Nothing -> Bad $ "Error: Main function not found."
    Just (t,args,_) -> if t /= Int then Bad $ "Error: Main function return type should be int."
        else if args /= [] then Bad $ "Error: Main function does not take any arguments."
        else return ()

-- Check if every function has a return (excl. void functions)

checkReturn :: Env -> Err ()
checkReturn env = mapM_ checkReturn' $ toList $ fEnv env

checkReturn' :: (Var, FVal) -> Err ()
checkReturn' (_,(Void,_,_)) = return ()
checkReturn' (var,(_,_,b)) = case checkReturnBlock b of
    Ok _  -> Bad $ "Error: Reached end of non-void function " ++ var ++ "."
    Bad _ -> return ()

checkReturnBlock :: Block -> Err ()
checkReturnBlock (Block stmts) = mapM_ checkReturnStmt stmts

checkReturnStmt :: Stmt -> Err ()
checkReturnStmt (Ret _) = Bad "Ok"
checkReturnStmt (VRet)  = Bad "Ok"
checkReturnStmt _       = return ()

-- Main error check function

checkProgram :: Program -> IO ()
checkProgram prog = case checkProgram' prog of
    Ok _  -> return ()
    Bad e -> putStrLn e

checkProgram' :: Program -> Err ()
checkProgram' prog = do
    env <- checkTopDef prog
    tcProg env
    checkReturn env
    checkMain env
    return ()

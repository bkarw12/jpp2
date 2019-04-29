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
type Loc = Int
data Val = VInt | VBool | VStr | VNone
    deriving (Eq)
type VVal = (Type, Val, Integer)
type FVal = (Type, [Arg], Block)

type LEnv = Map Var Loc
type VEnv = Map Loc VVal
type FEnv = Map Var FVal
type Env = (LEnv, VEnv, FEnv, Integer, Type)

-- Auxillary functions

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

-- TODO: generalize these getters?

getLoc :: Var -> StateT Env Err (Loc)
getLoc var = do
    (ls,_,_,_,_) <- get
    case Data.Map.lookup var ls of
        Nothing  -> lift $ Bad $ "Error: variable " ++ show var ++ " not declared."
        Just loc -> return loc

getVVal :: Loc -> StateT Env Err (VVal)
getVVal loc = do
    (_,vs,_,_,_) <- get
    case Data.Map.lookup loc vs of
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
insertArg (ls,vs,fs,n,ret) (Arg t (Ident var)) = (ls',vs',fs,n,ret)
    where   loc = newLoc vs
            ls' = insert var loc ls
            vs' = insert loc (t,typeToVal t,n) vs

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
    (ls,vs,fs,n,ret) <- get
    let loc = newLoc vs
    case Data.Map.lookup var ls of
        Nothing  -> return ()
        Just oldLoc -> do
            (_,_,m) <- getVVal oldLoc
            if n <= m then lift $ Bad $ "Error: Variable redeclaration: " ++ var ++ "."
            else return ()
    let ls' = insert var loc ls
        vs' = insert loc (t,val,n) vs
    put (ls',vs',fs,n,ret)

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
    (ls,vs,fs,n,ret) <- get
    loc <- getLoc var
    (t,_,num) <- getVVal loc
    let vs' = insert loc (t,typeToVal t,num) vs
    put (ls,vs',fs,n,ret)
        
-- Raw type checking

tcBlock :: Block -> StateT Env Err ()
tcBlock (Block stmts) = mapM_ tcStmt stmts

tcStmt :: Stmt -> StateT Env Err ()
tcStmt (BStmt b) = do
    (ls,vs,fs,n,ret) <- get
    put (ls,vs,fs,n+1,ret)
    tcBlock b
    (_,vs,fs,_,_) <- get
    put (ls,vs,fs,n,ret)
tcStmt (DeclStmt (Decl t items)) = mapM_ (checkInsertVar t) items
tcStmt (Ass id e) = do
    t <- tcExpr e
    checkInit e
    tcExpr'NoInit t (EVar id) $ "Error: Types mismatch in variable assignment: \"" ++ show id ++ "\"."
    let (Ident var) = id
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
    tcStmt stmt
tcStmt (While e stmt) = do
    tcExpr' Bool e $ "Error: While loop condition expression " ++ show e ++ " is not a boolean."
    tcStmt stmt
tcStmt (SExp e) = do 
    tcExpr e
    return ()
tcStmt (Ret e) = do
    (_,_,_,_,ret) <- get
    tcExpr' ret e $ "Error: Types mismatch in return: " ++ show (Ret e) ++ "."
tcStmt VRet = do
    (_,_,_,_,ret) <- get
    if ret == Void then return ()
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
    (_,_,fs,_,_) <- get
    case Data.Map.lookup var fs of
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
tcVars (ls,vs,_,_,_) = mapM_ (tcVars' ls) $ toList vs

tcVars' :: LEnv -> (Loc, VVal) -> Err ()
tcVars' ls (loc,vval) = case vval of
    (Int, VInt,_)   -> Ok ()
    (Bool, VBool,_) -> Ok ()
    (Str, VStr,_)   -> Ok ()
    _               -> Bad $ "Error: types mismatch for global variable \"" ++ (keyFromValue ls loc) ++ "\" assignment."

tcFuncs :: Env -> Err ()
tcFuncs env = mapM_ (tcFuncs' env) $ toList fs
    where (_,_,fs,_,_) = env

tcFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
tcFuncs' (ls,vs,fs,n,_) (_,(t,args,b)) = runStateT (tcBlock b) $ insertArgs env' args
    where env' = (ls,vs,fs,n,t)

-- Checking global declarations

checkTopDef :: Program -> Err Env
checkTopDef prog = case runStateT (checkTopDef' prog) (empty, empty, empty, 1, Void) of
    Ok ((),s) -> Ok s
    Bad e     -> Bad e 

checkTopDef' :: Program -> StateT Env Err ()
checkTopDef' (Program topdefs) = mapM_ checkTopDef'' topdefs

checkTopDef'' :: TopDef -> StateT Env Err ()
checkTopDef'' (FnDef t (Ident var) args b) = do
    (ls,vs,fs,n,ret) <- get
    if notMember var fs then 
        let fs' = insert var (t,args,b) fs 
        in put (ls,vs,fs',n,ret)
    else lift $ Bad $ "Error: Function redeclaration: " ++ var ++ "."
checkTopDef'' (VDef (Decl t items)) = mapM_ (checkTopDefV t) items

checkTopDefV :: Type -> Item -> StateT Env Err ()
checkTopDefV t (NoInit (Ident var)) = checkTopDefV' VNone t var
checkTopDefV t (Init (Ident var) e) = do
    val <- calcTopDefVal e
    checkTopDefV' val t var

checkTopDefV' :: Val -> Type -> Var -> StateT Env Err ()
checkTopDefV' val t var = do
    (ls,vs,fs,n,ret) <- get
    if notMember var ls then
        let loc = newLoc vs
            ls' = insert var loc ls
            vs' = insert loc (t,val,0) vs
        in put (ls',vs',fs,n,ret)
    else lift $ Bad $ "Error: Global variable redeclaration: " ++ var ++ "."

calcTopDefVal :: Expr -> StateT Env Err Val -- TODO add more expressions?
calcTopDefVal (ELitInt _)   = lift $ Ok VInt
calcTopDefVal ELitTrue      = lift $ Ok VBool
calcTopDefVal ELitFalse     = lift $ Ok VBool
calcTopDefVal (EString _)   = lift $ Ok VStr
calcTopDefVal _             = lift $ Bad $ "Error: Expression assigned to global variable is not a constant value."

-- Main function check

checkMain :: Env -> Err ()
checkMain (_,_,fs,_,_) = case Data.Map.lookup "main" fs of
    Nothing -> Bad $ "Error: Main function not found."
    Just (t,args,_) -> if t /= Int then Bad $ "Error: Main function return type should be int."
        else if args /= [] then Bad $ "Error: Main function does not take any arguments."
        else return ()

-- Check if every function has a return (excl. void functions)

checkReturn :: Env -> Err ()
checkReturn (_,_,fs,_,_) = mapM_ checkReturn' $ toList fs

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

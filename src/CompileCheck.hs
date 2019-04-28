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
insertArg (vs,fs) (Arg t (Ident var)) = (insert var (t,typeToVal t) vs,fs)

checkInsertVar :: Type -> Item -> StateT Env Err ()
checkInsertVar t (NoInit (Ident var)) = do
    (vs,fs) <- get
    let vs' = insert var (t,VNone) vs
    put (vs',fs) 
checkInsertVar t (Init (Ident var) e) = do
    te <- tcExpr e
    if t == te then do
        (vs,fs) <- get
        let vs' = insert var (t,typeToVal t) vs
        put (vs',fs)
    else lift $ Bad $ "Error: Types mismatch in variable declaration: \"" ++ var ++ "\"."

checkArgs :: [Arg] -> [Expr] -> String -> String -> StateT Env Err ()
checkArgs args es s1 s2
    | length args /= length es  = lift $ Bad s1
    | otherwise                 = mapM_ (checkArgs' s2) $ zip args es

checkArgs' :: String -> (Arg, Expr) -> StateT Env Err ()
checkArgs' s ((Arg t _), e) = tcExpr' t e s
        
-- Raw type checking

tcBlock :: Block -> StateT Env Err ()
tcBlock (Block stmts) = mapM_ tcStmt stmts

tcStmt :: Stmt -> StateT Env Err ()
tcStmt (BStmt b) = tcBlock b
tcStmt (DeclStmt (Decl t items)) = mapM_ (checkInsertVar t) items
tcStmt (Ass id e) = do
    t <- tcExpr e
    tcExpr' t (EVar id) $ "Error: Types mismatch in variable assignment: \"" ++ show id ++ "\"."
tcStmt (Incr id) = tcExpr' Int (EVar id) $ "Error: Types mismatch in variable incrementation: \"" ++ show id ++ "\"."
tcStmt (Decr id) = tcExpr' Int (EVar id) $ "Error: Types mismatch in variable decrementation: \"" ++ show id ++ "\"."
tcStmt (Cond e stmt) = do 
    tcExpr' Bool e $ "Error: Condition expression " ++ show e ++ " is not a boolean."
    tcStmt stmt
tcStmt (CondElse e stmt1 stmt2) = do
    tcStmt (Cond e stmt1)
    tcStmt stmt2
tcStmt (For id e1 e2 stmt) = do
    tcExpr' Int (EVar id) $ "Error: For loop argument " ++ show id ++ " is not an int."
    tcExpr' Int e1 $ "Error: For loop start expression " ++ show e1 ++ " is not an int."
    tcExpr' Int e2 $ "Error: For loop end expression " ++ show e1 ++ " is not an int."
    tcStmt stmt
tcStmt (While e stmt) = do
    tcExpr' Bool e $ "Error: While loop condition expression " ++ show e ++ " is not a boolean."
    tcStmt stmt
tcStmt (SExp e) = do 
    tcExpr e
    return ()
tcStmt _ = return ()

tcExpr :: Expr -> StateT Env Err Type
tcExpr (EVar (Ident var)) = do
    (vs,_) <- get
    case Data.Map.lookup var vs of
        Nothing     -> lift $ Bad $ "Error: variable \"" ++ show var ++ "\" not declared."
        Just (t,_)  -> return t
tcExpr (ELitInt _) = return Int
tcExpr ELitTrue = return Bool
tcExpr ELitFalse = return Bool
tcExpr (EApp (Ident var) es) = do
    (_,fs) <- get
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
tcVars (vs,_) = mapM_ tcVars' $ toList vs

tcVars' :: (Var, VVal) -> Err ()
tcVars' (var,vval) = case vval of
    (Int, VInt)   -> Ok ()
    (Bool, VBool) -> Ok ()
    (Str, VStr)   -> Ok ()
    _               -> Bad $ "Error: types mismatch for global variable \"" ++ var ++ "\" assignment."

tcFuncs :: Env -> Err ()
tcFuncs env = mapM_ (tcFuncs' env) $ toList fs
    where (_,fs) = env

tcFuncs' :: Env -> (Var, FVal) -> Err ((), Env)
tcFuncs' env (_,(_,args,b)) = runStateT (tcBlock b) $ insertArgs env args

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
    else lift $ Bad $ "Error: Function redeclaration: " ++ var ++ "."
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
    else lift $ Bad $ "Error: Global variable redeclaration: " ++ var ++ "."

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
    tcProg env
    return ()

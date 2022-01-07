{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StaticAnalysis where
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Debug.Trace
import System.IO
import AbsLatte
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import qualified Data.Map.Lazy as M
import Data.List (isPrefixOf, findIndex, elemIndex)
import Data.Set as S (Set, insert, member, empty)
import Control.Exception (throw)
import Data.Maybe

-- TYPES, DATA, MONAD
type Depth = Integer
data VarD = VarD { vType :: Type, vDepth :: Depth}
data FunD = FunD { fVenv :: VEnv, fRetType :: Type, fArgs :: [Arg], fBlock :: Block}
type SAM a = ExceptT StaticError (State Env) a
type Env = (VEnv, FEnv, Depth, (Ident, Type))
type VEnv = M.Map Ident VarD
type FEnv = M.Map Ident FunD
initEnv = (M.empty, M.empty, 0, (Ident "main", Int internalPos))

type StaticError = StaticError' BNFC'Position
data StaticError' a = RepeatedArgName String a |
    MissingFnRetType Ident  a| Undefined Ident  a| Redeclaration Ident  a|
    MissingRet Ident  a| TypesMismatch Type Type  a|  WrongFnRetType Ident Type Type  a|
    WrongArgType Ident Arg Type a| WrongArgNumber Ident Int Int  a| InvalidOperation  a|
    WrongMainArgNumber Int  a| WrongMainRetType Type  a

instance Show StaticError where
    show (RepeatedArgName s p) = "Reapeated argument in " ++ s ++ inLine p
    show (MissingFnRetType (Ident i) p) = "Function " ++ i ++ " is missing a return type" ++ inLine p
    show (Undefined (Ident i) p) = "Undefined symbol " ++ i ++ inLine p
    show (Redeclaration (Ident i)  p) = "Redaclaration of " ++ i ++ inLine p
    show (MissingRet (Ident i) p) = "Function " ++ i ++ " is missing a return statement" ++ inLine p
    show (TypesMismatch t1 t2 p) = "Expected type " ++ myShow t1 ++ ", got " ++ myShow t2 ++ inLine p
    show (WrongFnRetType (Ident i) t1 t2 p) = "Function " ++ i ++ " expects a return of type " ++ myShow t1 ++ " got " ++ myShow t2 ++ inLine p
    show (WrongArgType (Ident i) (Arg p1 t1 (Ident a)) t2 p2) = "Function " ++ i ++ " expects an argument " ++ a ++ " of type " ++ myShow t1 ++ " but received " ++ myShow t2 ++ inLine p2
    show (WrongArgNumber (Ident i) i1 i2 p) = "Function " ++ i ++ " expects " ++ show i1 ++ " arguments " ++ " but received " ++ show i2 ++ inLine p
    show (InvalidOperation p) = "Invalid operation " ++ inLine p
    show (WrongMainArgNumber i p) = "Main function has to have 0 arguments, not " ++ show i ++ inLine p
    show (WrongMainRetType t p) = "Main function has to have int type, not " ++ myShow t ++ inLine p

myShow :: Type -> String
myShow (Int _) = "Int"
myShow (Str _) = "Str"
myShow (Bool _) = "Bool"
myShow (Void _) = "Void"
myShow (Fun _ _ _) = "Fun"


inLine :: BNFC'Position -> String 
inLine (Just (n1, n2)) = " in line " ++ show n1 ++ " on position " ++ show n2
inLine Nothing = ""

internalPos :: BNFC'Position
internalPos = Just(-42, -42)
predPos :: BNFC'Position
predPos = Just (-1,-1) -- declared before the program body
predefinedFuns :: [(Type, Ident, [Arg], Block)]
predefinedFuns = [(Void predPos, Ident "printInt", [Arg predPos (Int predPos) (Ident "n")], Block predPos [Empty predPos]),
            (Void predPos, Ident "printString", [Arg predPos (Str predPos) (Ident "str")], Block predPos [Empty predPos]),
            (Int predPos , Ident "readInt", [], Block predPos [Empty predPos]),
            (Str predPos, Ident "readString", [], Block predPos [Empty predPos]),
            (Void predPos, Ident "error", [], Block predPos [Empty predPos])]

-- EXPRESSIONS
getExprType :: Expr -> SAM Type
getExprType expr = case expr of
    EVar p ident -> getVarType ident p
    ELitInt p i -> return $ Int p
    ELitFalse pos -> return $ Bool pos
    ELitTrue pos -> return $ Bool pos
    EString pos _ -> return $ Str pos
    Neg p expr -> do
        t <- getExprType expr -- only int
        case t of
            (Int _) -> return t
            _       ->  throwE $ InvalidOperation p
    Not p expr -> do
        t <- getExprType expr
        case t of
            (Bool _) -> return t
            _       ->  throwE $ InvalidOperation p
    EMul p expr1 _ expr2 -> do
        t1 <- getExprType expr1
        t2 <- getExprType expr2
        case (t1, t2) of
            (Int _, Int _) -> return $ Int p
            _ -> throwE $ InvalidOperation p
    EAdd p expr1 _ expr2 -> do
        t1 <- getExprType expr1
        t2 <- getExprType expr2
        case (t1, t2) of
            (Int _, Int _) -> return $ Int p
            (Str _, Str _) -> return $ Str p
            _ -> throwE $ InvalidOperation p
    ERel p expr1 op expr2 -> do
        t1 <- getExprType expr1
        t2 <- getExprType expr2
        if op == EQU (hasPosition op) || op == NE (hasPosition op)
            then if t1 === t2
                then return $ Bool p
                else throwE $ InvalidOperation p
            else case (t1, t2) of
                (Int _, Int _) -> return $ Bool p
                _ -> throwE $ InvalidOperation p
    EAnd p expr1 expr2 -> do
        t1 <- getExprType expr1
        t2 <- getExprType expr2
        case (t1, t2) of
            (Bool _, Bool _) -> return $ Bool p
            _ -> throwE $ InvalidOperation p
    EOr p expr1 expr2 -> do
        t1 <- getExprType expr1
        t2 <- getExprType expr2
        case (t1, t2) of
            (Bool _, Bool _) -> return $ Bool p
            _ -> throwE $ InvalidOperation p
    EApp p ident exprs -> do
        fn <- getFnType ident p
        exprsTypes <- mapM getExprType exprs
        argTypes <- mapM getArgType (fArgs fn)
        let l1 = length exprsTypes
            l2 = length argTypes in
                when (l1 /= l2) (throwE $ WrongArgNumber ident l1 l2 p)
        case elemIndex False (zipWith (===) argTypes exprsTypes) of
            Just i -> throwE $ WrongArgType ident (fArgs fn!!i) (exprsTypes!!i) p
            Nothing -> return (fRetType fn)

-- STMTS
analyzeDecl :: Type -> Item -> SAM ()
analyzeDecl t (NoInit p ident) = do
    checkRedeclaration ident p
    (_, _, depth, _) <- get
    declareVar t ident depth
analyzeDecl ltype (Init p ident exp) = do
    checkRedeclaration ident p
    rtype <- getExprType exp
    unless (ltype === rtype) $ throwE $ TypesMismatch ltype rtype p
    (_, _, depth, _) <- get
    declareVar ltype ident depth

analyzeStmt :: Stmt -> SAM (Maybe Type) -- type of x from "return x;" 
analyzeStmt (VRet p) = do
    return $ Just (Void p)
analyzeStmt (BStmt p (Block p2 stmts)) = do
    env <- get
    incDepth
    t <- analyzeStmtsInBlock stmts p2
    put env
    return t
analyzeStmt (Decl p t items) = do
    mapM_ (analyzeDecl t) items
    return Nothing
analyzeStmt (Ret p expr) = do
    t <- getExprType expr
    return $ Just t
analyzeStmt (SExp p expr) = do
    _ <- getExprType expr
    return Nothing
analyzeStmt (Empty p) =
    return Nothing
analyzeStmt (Ass p ident expr) = do
    t1 <- getVarType ident p
    t2 <- getExprType expr
    unless (t1 === t2) (throwE $ TypesMismatch t1 t2 p)
    return Nothing
analyzeStmt (Incr p ident) = do
    t <- getVarType ident p
    case t of
        (Int _) -> return Nothing
        _ -> throwE $ InvalidOperation p
analyzeStmt (Decr p ident) = do
    t <- getVarType ident p
    case t of
        (Int _) -> return Nothing
        _ -> throwE $ InvalidOperation p
analyzeStmt (Cond p expr stmt) = do
    condType <- getExprType expr
    case (expr, condType) of
        (ELitFalse _, _) -> return Nothing
        (ELitTrue  _, _) -> do
            t <- analyzeStmt stmt
            retTypeMatchingFnType t p
            return t
        (_, Bool _) -> do
            t <- analyzeStmt stmt
            retTypeMatchingFnType t p
            return Nothing
        (_, _) -> throwE $ TypesMismatch (Bool p) condType p
analyzeStmt (CondElse p expr stmt1 stmt2) = do
    t <- getExprType expr
    case (expr, t) of
        (ELitTrue  _, Bool _) -> do
            t <- analyzeStmt stmt1
            retTypeMatchingFnType t p
            return t
        (ELitFalse _, Bool _) -> do
            t <- analyzeStmt stmt2
            retTypeMatchingFnType t p
            return t
        (_, Bool _) -> do
            t1 <- analyzeStmt stmt1
            retTypeMatchingFnType t1 p
            t2 <- analyzeStmt stmt2
            retTypeMatchingFnType t2 p
            case (t1, t2) of
                (Nothing, _) -> return Nothing
                (_, Nothing) -> return Nothing
                (_, _)       -> return t1 -- t1 === t2 === retFnType
        (_, _) -> throwE $ TypesMismatch (Bool p) t p
analyzeStmt (While p expr stmt) = analyzeStmt (Cond p expr stmt)

analyzeStmtsInBlock :: [Stmt] -> BNFC'Position  -> SAM (Maybe Type)
analyzeStmtsInBlock [] p = do
    retTypeMatchingFnType Nothing p
    return Nothing
analyzeStmtsInBlock (s:stmts) p = do
            t <- analyzeStmt s
            retTypeMatchingFnType t p
            if isNothing t
                then analyzeStmtsInBlock stmts p
                else return t

analyzeTopDef :: TopDef -> SAM ()
analyzeTopDef (FnDef p retType ident args (Block p2 stmts)) = do
    if ident == Ident "main"
        then checkMain retType args
        else checkRepeatedArgs args
    (venv, fenv, depth, fn) <- get
    put (venv, fenv, depth, (ident, retType))
    mapM_ declareArg args
    t <- analyzeStmtsInBlock stmts p2
    case t of
        Nothing -> unless (isVoid retType) (throwE $ WrongFnRetType ident retType (Void p) p)
        Just t' -> unless (t' === retType) (throwE $ WrongFnRetType ident retType t' p)
    put (venv, fenv, depth, fn)

statAnalyzeProgram :: Program -> SAM ()
statAnalyzeProgram (Program _ topDefs) = do
    mapM_ (\(t, i, a, b) -> registerFn t i a b) predefinedFuns -- register predefined
    mapM_ (\(FnDef p t i a b) -> registerFn t i a b) topDefs   -- register user-defined functions
    catchE (mapM_ analyzeTopDef topDefs) throwE

-- STARTING POINT
statAnalyze :: Program -> Maybe String
statAnalyze program = case evalState (runExceptT (statAnalyzeProgram program)) initEnv of
    (Right _) -> Nothing
    (Left err) -> Just (show err)

-- CHECK FOR SPECIFIC ERROR
retTypeMatchingFnType :: Maybe Type -> BNFC'Position -> SAM ()
retTypeMatchingFnType returnedType p = do
    (_, _, _, fn) <- get
    case returnedType of
        Nothing  -> return () -- no return is always correct
        Just (Void p2) -> unless (isVoid (snd fn)) (throwE $ WrongFnRetType (fst fn) (snd fn) (Void p2) p)
        (Just t) -> unless (snd fn === t) (throwE $ WrongFnRetType (fst fn) (snd fn) t p)

checkRedeclaration :: Ident -> BNFC'Position -> SAM ()
checkRedeclaration ident p = do
    (venv, _, depth, _) <- get
    case M.lookup ident venv of
        Just var -> when (vDepth var == depth) $ throwE $ Redeclaration ident p
        Nothing -> return ()

checkMain :: Type -> [Arg] -> SAM ()
checkMain t args = do
    case (t, args) of
        (Int _, []) -> return ()
        (Int _, _)  -> throwE $ WrongMainArgNumber (length args) (hasPosition (head args))
        (_, _) -> throwE $ WrongMainRetType t (hasPosition t)


checkRepeatedArgs :: [Arg] -> SAM ()
checkRepeatedArgs [] = return ()
checkRepeatedArgs args =
    let names = [n | (Arg _ _ (Ident n)) <- args]
        ds = duplicates S.empty names in
            unless (null ds) (throwE $ RepeatedArgName (head ds) (hasPosition (head args)))

-- TYPE UTILS 
isVoid :: Type -> Bool
isVoid t = case t of
    (Void _) -> True
    _ -> False

getVarType :: Ident -> BNFC'Position -> SAM Type
getVarType ident p = do
    (venv, _, _, _) <- get
    case M.lookup ident venv of
        Just var -> return $ vType var
        Nothing -> throwE $ Undefined ident p

getArgType :: Arg -> SAM Type
getArgType (Arg _ t _) =
    return t


getFnType :: Ident -> BNFC'Position -> SAM FunD
getFnType ident p = do
    (_, fenv, _, _) <- get
    case M.lookup ident fenv of
        Just fn -> return fn
        Nothing -> throwE $ Undefined ident p

(===) :: Type -> Type -> Bool
Int _ === Int _                         = True
Str _ === Str _                         = True
Bool _ === Bool _                       = True
Fun _ rt1 at1 === Fun _ rt2 at2         = rt1 === rt2 && length at1 == length at2 && all (uncurry (===)) (zip at1 at2)
Void _ === Void _                       = True
_ === _                                 = False

(=/=) :: Type -> Type -> Bool
t1 =/= t2 = not $ t1 === t2

-- UTILS --
duplicates :: Ord a => Set a -> [a] -> [a]
duplicates seen [] = []
duplicates seen (x:xs) = if x `S.member` seen
    then [x]
    else duplicates (S.insert x seen) xs

-- MONAD UTILS
registerFn :: Type -> Ident -> [Arg] -> Block -> SAM ()
registerFn t ident args b = do
    (venv, fenv, depth, fn) <- get
    put (venv, M.insert ident FunD {fVenv=venv, fRetType=t, fArgs=args, fBlock=b} fenv, depth, fn)
    return ()

declareArg :: Arg -> SAM ()
declareArg (Arg _ t id) = do
    d <- getDepth
    declareVar t id d

declareVar :: Type -> Ident -> Depth -> SAM ()
declareVar t id d = do
    (venv, fenv, depth, fn) <- get
    put (M.insert id VarD {vType=t, vDepth=d} venv, fenv, depth, fn)
    return ()

getDepth :: SAM Depth
getDepth = do
    (_, _, d, _) <- get
    return d

incDepth :: SAM ()
incDepth = do
    (venv, fenv, d, fn) <- get
    put (venv, fenv, d+1, fn)
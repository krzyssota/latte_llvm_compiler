{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleInstances #-}

module CompileToLLVM where
import LLVMDomain as LLVM
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
import Data.List (isPrefixOf, findIndex, elemIndex, intercalate)
import Data.Set as S (Set, insert, member, empty)
import Control.Exception (throw)
import Data.Maybe
import System.Exit (exitFailure)
import Data.Functor.Classes (eq1)
import Distribution.Compat.Lens (_1)
import Distribution.PackageDescription.Check (checkConfiguredPackage)

type VarsEnv = M.Map Ident (LLVM.Type, Loc)
initialEnv = M.empty
type FnEnv = M.Map String LLVM.Type
data CompilerState = CompilerState {
    registerCounter :: Counter,
    globalsCounter :: Counter,
    globInstructions :: [Instruction],
    instructions :: [Instruction],
    functions :: FnEnv
} deriving (Show)
initialCompilerState :: CompilerState
initialCompilerState = CompilerState {
    registerCounter = newCounter,
    globalsCounter = newCounter,
    globInstructions = [],
    instructions = [],
    functions = M.empty
}
type CompilerM = ReaderT VarsEnv (ExceptT MyError (StateT CompilerState IO))
type RetInStmt = Bool
type CompStmtRes = (VarsEnv, RetInStmt)
defaultRes :: CompilerM  CompStmtRes
defaultRes = do
    env <- ask
    return (env, False)
newtype Counter = Counter Integer
instance Show Counter where
    show (Counter i) = "Counter " ++ show i
newCounter :: Counter
newCounter = Counter 1
incCounter :: Counter -> Counter
incCounter (Counter i) = Counter $ i+1
castCounter :: Counter -> Integer
castCounter (Counter i) = i
newRegister :: CompilerM Integer -- TODO niech zwraca (Register old)
newRegister = do
    cs <- get
    let old = castCounter (registerCounter cs) in do
        modify (\s -> s {registerCounter = incCounter $ registerCounter cs})
        return old
newLabel :: CompilerM Integer
newLabel = newRegister
newGlobal :: CompilerM Integer
newGlobal = do
    cs <- get
    let old = castCounter (globalsCounter cs) in do
        modify (\s -> s {globalsCounter = incCounter $ globalsCounter cs})
        return old

addInstruction :: Instruction -> CompilerM ()
addInstruction ins = do
    cs <- get
    modify (\s -> s {instructions = ins:instructions cs})

addGlobalInstruction :: Instruction -> CompilerM ()
addGlobalInstruction ins = do
    cs <- get
    modify (\s -> s {globInstructions = ins:globInstructions cs})

registerFn :: TopDef -> CompilerM ()
registerFn (FnDef p1 t (Ident ident) args _) = do
    cs <- get
    modify (\s -> s {functions = M.insert ident (LLVM.llvmType t) (functions cs)})

getFnType :: String -> CompilerM LLVM.Type
getFnType ident = do
    cs <- get
    let funs = functions cs in
        case M.lookup ident funs of
            Just t -> return t
            Nothing -> throwError $ FrontBug ("no such ident " ++ ident)


type MyError = MyError' BNFC'Position
data MyError' a = NotImplemented String | FrontBug String | Debug String
instance Show MyError where
    show (NotImplemented s) = "NotImplemented " ++ s
    show (FrontBug s) = "FrontBug " ++ s
    show (Debug s) = "Debug " ++ s
internalPos :: BNFC'Position
internalPos = Just(-42, -42)

runCompilerM :: CompilerM a -> IO (Either MyError a, CompilerState)
runCompilerM e = runStateT (runExceptT (runReaderT e initialEnv)) initialCompilerState

compile :: Program -> IO ()
compile tree = do
    (err, state) <- runCompilerM (compile' tree)
    case err of
      Left e -> do
          putStrLn ( "managed to compile before error\n" ++
                    intercalate "\n" (map show (globInstructions state))
                    ++ "\n" ++ prologue
                    ++ intercalate "\n" (map show (reverse $ instructions state)))
          hPrint stderr e
          exitFailure
      Right _ -> putStrLn (
                    intercalate "\n" (map show (globInstructions state))
                    ++ "\n" ++ prologue
                    ++ intercalate "\n" (map show (reverse $ instructions state)))

{- clearEmptyBlocks :: [Instruction] -> [Instruction]
clearEmptyBlocks -}

compile' :: Program -> CompilerM ()
compile' (Program _ topDefs) = do
    mapM_ registerBuiltInFn builtInFuns
    mapM_ registerFn topDefs
    mapM_ compileTopDef topDefs

registerBuiltInFn :: (String, LLVM.Type) -> CompilerM ()
registerBuiltInFn (ident, t) = do
    cs <- get
    modify (\s -> s {functions = M.insert ident t (functions cs)})


compileTopDef :: TopDef -> CompilerM ()
compileTopDef (FnDef p1 t (Ident ident) args (Block p2 stmts)) = do
    funArgs <- mapM (funArg . llvmArg) args
    case ident of
        "main" -> do
            addInstruction DefineMain
            compileStmts stmts
            addInstruction ClosingBracket
        _ ->  do
            addInstruction $ Define (llvmType t) ident (map funDefArg funArgs)
            env <- declareFunArgs funArgs
            local (const env) $ compileStmts stmts
            when (llvmType t == LLVM.Void) (addInstruction RetVoid)
            addInstruction ClosingBracket

funDefArg :: (LLVM.Type, Ident, Register) -> (LLVM.Type, Register) 
funDefArg (t, _, r) = (t, r)

funArg :: (LLVM.Type, Ident) -> CompilerM (LLVM.Type, Ident, Register)
funArg (t, ident) = do
    r <- newRegister
    return (t, ident, Register r)

declareFunArg :: (LLVM.Type, Ident, Register) -> CompilerM VarsEnv
declareFunArg (t, ident, r1) = do
    r2 <- newRegister
    addInstruction (LLVM.Alloc (Register r2) t)
    addInstruction (LLVM.Store (VRegister r1) t (Register r2))
    asks $ M.insert ident (t, r2)

declareFunArgs :: [(LLVM.Type, Ident, Register)] -> CompilerM VarsEnv
declareFunArgs [] = ask
declareFunArgs (arg:funArgs) = do
    env <- declareFunArg arg
    local (const env) $ declareFunArgs funArgs


compileStmts :: [Stmt] -> CompilerM CompStmtRes
compileStmts [] = defaultRes
compileStmts (s:stmts) = do
    (env, ret1) <- compileStmt s
    (env2, ret2) <- local (const env) (compileStmts stmts)
    return (env2, ret1 || ret2)

compileStmt :: Stmt -> CompilerM CompStmtRes
compileStmt (Empty p) = defaultRes
compileStmt (SExp p expr) = do
        compileExpr expr
        defaultRes
compileStmt (AbsLatte.Ret p expr) = do
    (t, v) <- compileExpr expr
    case v of
        Nothing -> throwError $ FrontBug "bad ret"
        Just val -> do
            addInstruction (LLVM.Ret t val)
            env <- ask
            return (env, True)
compileStmt (AbsLatte.VRet p) = do
    addInstruction LLVM.RetVoid
    env <- ask
    return (env, True)
compileStmt (Decl p t (it:items)) = do
    env <- declareVar t it
    --throwError $ Debug (show env)
    local (const env) $ compileStmt (Decl p t items)
    --env2 <- ask
    --throwError $ Debug (show env2) -- TODO usunac
compileStmt (Decl p t []) = defaultRes
compileStmt (BStmt p1 (Block p2 stmts)) = do
    env <- ask
    (_, ret) <- local (const env) (compileStmts stmts)
    return (env, ret)
compileStmt (Ass p1 ident expr) = do
    (t, v) <- compileExpr expr
    (declaredT, loc) <- getVar ident
    when (t /= declaredT) (throwError $ FrontBug "ass with wrong type")
    case v of
        Just val -> do
            --addInstruction (LLVM.Store val t loc)
            addInstruction (LLVM.Store val t (Register loc))

            defaultRes
        Nothing -> throwError $ FrontBug "assigning void expr to var"
compileStmt (Incr _ ident) = do
    r1 <- newRegister
    r2 <- newRegister
    (t, l) <- getVar ident
    --addInstruction $ LLVM.Load (Register r1) t l
    addInstruction $ LLVM.Load (Register r1) t (Register l)
    addInstruction $ LLVM.Ari (Register r2) LLVM.Add (VRegister $ Register r1) (VConst (ConstI 1))
    --addInstruction $ LLVM.Store (VRegister $ Register r2) t l
    addInstruction $ LLVM.Store (VRegister $ Register r2) t (Register l)

    defaultRes
compileStmt (Decr _ ident) = do
    r1 <- newRegister
    r2 <- newRegister
    (t, l) <- getVar ident
    --addInstruction $ LLVM.Load (Register r1) t l
    addInstruction $ LLVM.Load (Register r1) t (Register l)
    addInstruction $ LLVM.Ari (Register r2) LLVM.Add (VRegister $ Register r1) (VConst (ConstI (-1)))
    --addInstruction $ LLVM.Store (VRegister $ Register r2) t l
    addInstruction $ LLVM.Store (VRegister $ Register r2) t (Register l)
    defaultRes
compileStmt (Cond _ expr stmt) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            thenLabel <- newLabel
            afterLabel <- newLabel
            addInstruction $ LLVM.BrCond val (Label thenLabel) (Label afterLabel)
            addInstruction $ LLVM.ILabel (Label thenLabel)
            compileStmt stmt
            addInstruction $ LLVM.Br (Label afterLabel)
            addInstruction $ LLVM.ILabel (Label afterLabel)
            defaultRes
        _ -> throwError $ FrontBug "not bool cond in if"
    -- TODO to, moze osobne compileCond w ktorym podaje labelki jako argumenty. i wykorzystać te funkcje teź w compileExpr And/Or
{- compileStmt (CondElse _ (Not _ expr) stmt) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I8, Just val) -> do -}
compileStmt (CondElse _ expr s1 s2) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            thenLabel <- newLabel
            elseLabel <- newLabel
            afterLabel <- newLabel
            addInstruction $ LLVM.BrCond val (Label thenLabel) (Label elseLabel)
            addInstruction $ LLVM.ILabel (Label thenLabel)
            (_, ret1) <- compileStmt s1
            --throwError $ Debug ("then " ++ show s1 ++ show ret1)
            unless ret1 (addInstruction $ LLVM.Br (Label afterLabel))
            addInstruction $ LLVM.ILabel (Label elseLabel)
            (_, ret2) <- compileStmt s2  -- TODO zmienic nazywanie labelek na "Label_i:"
            unless ret2 (addInstruction $ LLVM.Br (Label afterLabel)) -- TODO sprawdzac czy obie galęzie returnują. wtedy nie trzeba skakać i miec label after
            when (not ret1 || not ret2) (addInstruction $ LLVM.ILabel (Label afterLabel)) -- TODO co jeśli ju nic nie ma po if else? daje nam to pusty blok
            env <- ask
            return (env, ret1 && ret2)
        _ -> throwError $ FrontBug "not bool cond in ifelse"
compileStmt (While _ expr stmt) = do
    bodyLabel <- newLabel
    checkCondLabel <- newLabel
    afterLabel <- newLabel
    addInstruction $ LLVM.Br (Label checkCondLabel)
    addInstruction $ LLVM.ILabel (Label bodyLabel)
    (_, ret) <- compileStmt stmt
    --when (not ret) (newLabel >>= (\afterLabel -> addInstruction $ LLVM.Br (Label afterLabel)))
    addInstruction $ LLVM.Br (Label afterLabel) -- there is always after label because ret in while is not for certain
    addInstruction $ LLVM.ILabel (Label checkCondLabel)
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            addInstruction $ LLVM.BrCond val (Label bodyLabel) (Label afterLabel)
            addInstruction $ LLVM.ILabel (Label afterLabel)
            defaultRes
        _ -> throwError $ FrontBug "not bool cond in while"


{-
if (cond){
	return 1;
} else {
	return 0;
}


define i32 @f() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8, align 1
  %3 = load i8, i8* %2, align 1
  %4 = trunc i8 %3 to i1
  br i1 %4, label %5, label %6

5:                                                ; preds = %0
  store i32 1, i32* %1, align 4
  br label %7

6:                                                ; preds = %0
  store i32 0, i32* %1, align 4
  br label %7

7:                                                ; preds = %6, %5
  %8 = load i32, i32* %1, align 4
  ret i32 %8
}


int main() {
    bool b1 = true, b2 = false;
    if (b1 || b2) {
        return ;
    } else {
        return ;
    }
}

define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8, align 1
  %3 = alloca i8, align 1
  store i32 0, i32* %1, align 4
  store i8 1, i8* %2, align 1     ; b1 = true
  store i8 0, i8* %3, align 1     ; b2 = false;

  %4 = load i8, i8* %2, align 1
  %5 = trunc i8 %4 to i1
  br i1 %4, label %8, label %5    ; if b1 jmp 8 already_good else jmp 5 check_more

5: check_more                                               ; preds = %0
  %6 = load i8, i8* %2, align 1
  %7 = trunc i8 %6 to i1
  br i1 %7, label %8, label %9   ; if b2 jmp 8 good else jmp 9 else

8: if already good                                                ; preds = %5, %0
  br label %10                  ; jmp 10 end

9: else                                                ; preds = %5
  br label %10                  ; step 10 end

10:                                               ; preds = %9, %8
  ret void

-}

getVar :: Ident -> CompilerM (LLVM.Type, Loc)
getVar ident = do
    env <- ask
    case M.lookup ident env of
        Just (t, l) -> return (t, l)
        Nothing -> throwError $ FrontBug "get undeclared bar"

declareVar :: AbsLatte.Type -> Item -> CompilerM VarsEnv
declareVar t item = do
    n <- newRegister
    addInstruction (LLVM.Alloc (Register n) (llvmType t))
    case item of
        Init p ident expr -> do
            (t2, v) <- compileExpr expr
            when (llvmType t /= t2) (throwError $ FrontBug "decl mismatched types")
            case v of
                Just v -> do
                    --addInstruction (LLVM.Store v t2 n)
                    addInstruction (LLVM.Store v t2 (Register n))
                    env <- ask
                    return (M.insert ident (llvmType t, n) env)
                Nothing -> throwError $ FrontBug "void item in decl"

        NoInit p ident -> do
            --asks (M.insert ident (llvmType t, n))
            env <- ask
            return (M.insert ident (llvmType t, n) env)

compileExpr :: Expr -> CompilerM (LLVM.Type, Maybe Value)
compileExpr (ELitInt p i) =
        return (I32, Just (VConst (ConstI i)))
compileExpr (EApp p ident exprs) = do
        args_ <- mapM compileExpr exprs
        args <- mapM unJustify args_
        call ident args
compileExpr (ELitTrue p) =
    return (I1, Just (VConst ConstT))
compileExpr (ELitFalse p) =
    return (I1, Just (VConst ConstF))
compileExpr (EVar p ident) = do
    n <- newRegister
    env <- ask
    case M.lookup ident env of
        Just (t, ptr) -> do
            --addInstruction (Load (Register n) t ptr)
            addInstruction (Load (Register n) t (Register ptr))
            return (t, Just $ VRegister (Register n))
        Nothing -> throwError $ FrontBug "undefined var"
compileExpr (EString p s) = do
    n <- newGlobal
    addGlobalInstruction $ DeclareGString n s (length s + 1)
    return (LLVM.Ptr I8, Just $ GetElemPtr n (length s + 1))
compileExpr (EAdd p e1 addOp e2) = do
    (t1, v1) <- compileExpr e1
    (t2, v2) <- compileExpr e2
    case (t1, v1, t2, v2) of
        (_, Nothing, _, _) -> throwError $ FrontBug "ari op with void expr"
        (_, _, _, Nothing) -> throwError $ FrontBug "ari op with void expr"
        (I32, Just val1, I32, Just val2) -> do
                r <- newRegister
                addInstruction $ LLVM.Ari (Register r) LLVM.Add val1 val2
                return (t1, Just $ VRegister (Register r))
        (Ptr I8, Just val1, Ptr I8, Just val2) ->
                call (Ident "concatStrings") [(t1, val1), (t2, val2)]
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (EMul p e1 addOp e2) = do
    (t1, v1) <- compileExpr e1
    (t2, v2) <- compileExpr e2
    case (t1, v1, t2, v2) of
        (_, Nothing, _, _) -> throwError $ FrontBug "ari op with void expr"
        (_, _, _, Nothing) -> throwError $ FrontBug "ari op with void expr"
        (I32, Just val1, I32, Just val2) -> do
                r <- newRegister
                addInstruction $ LLVM.Ari (Register r) LLVM.Mul val1 val2
                return (t1, Just $ VRegister (Register r))
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (ERel _ e1 relOp e2) = do
    (t1, v1) <- compileExpr e1
    (t2, v2) <- compileExpr e2
    case (t1, v1, t2, v2) of
        (_, Nothing, _, _) -> throwError $ FrontBug "ari op with void expr"
        (_, _, _, Nothing) -> throwError $ FrontBug "ari op with void expr"
        (I32, Just val1, I32, Just val2) -> do
                r <- newRegister
                addInstruction $ LLVM.Cmp (Register r) (llvmRelOp relOp) I32 val1 val2
                return (I1, Just $ VRegister (Register r))
        (Ptr I8, Just val1, Ptr I8, Just val2) -> do
            case relOp of
                EQU _ -> do
                    (_, v) <- call (Ident "equStrings") [(t1, val1), (t2, val2)]
                    return (I1, v)
                AbsLatte.NE _ -> do
                    (_, v) <- call (Ident "neStrings") [(t1, val1), (t2, val2)]
                    return (I1, v)
                _ -> throwError $ FrontBug "string relOp other than EQU NE"
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (Neg _ expr) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I32, Just val) -> do
            r <- newRegister
            addInstruction $ LLVM.Ari (Register r) LLVM.Sub (VConst (ConstI 0)) val
            return (I32, Just $ VRegister (Register r))
        (_, Just val) -> throwError $ FrontBug "neg of not i32 type"
        (_, Nothing) -> throwError $ FrontBug "neg of void expr"
compileExpr (Not _ expr) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            r <- newRegister
            addInstruction $ LLVM.Xor (Register r) val (VConst ConstT)
            return (I1, Just $ VRegister (Register r))
        (_, Just val) -> throwError $ FrontBug "not of not i1 type"
        (_, Nothing) -> throwError $ FrontBug "not of void expr"
compileExpr (EAnd _ e1 e2) = do -- TODO przetestować jak bedzie if
    entryLabel <- newLabel
    addInstruction $ LLVM.Br (Label entryLabel) -- maybe add currLabel: Maybe Label, in CompilerState to add if neccesarry
    addInstruction $ LLVM.ILabel (Label entryLabel)
    (t1, v1) <- compileExpr e1
    case (t1, v1) of
        (I1, Just val1) -> do
            labelExpr1True <- newLabel
            labelAfter <- newLabel
            addInstruction $ LLVM.BrCond val1 (Label labelExpr1True) (Label labelAfter)
            addInstruction $ LLVM.ILabel (Label labelExpr1True)
            (t2, v2) <- compileExpr e2
            case (t2, v2) of
                (I1, Just val2) -> do
                    addInstruction $ LLVM.Br (Label labelAfter)
                    addInstruction $ LLVM.ILabel (Label labelAfter)
                    n <- newRegister
                    addInstruction $ LLVM.Phi (Register n) I1 [(VConst ConstF, Label entryLabel), (val2, Label labelExpr1True)]
                    return (I1, Just $ VRegister (Register n))
                _ -> throwError $ FrontBug "true %% void e2"
        _ -> throwError $ FrontBug "void e1 %% e2"
compileExpr (EOr _ e1 e2) = do
    entryLabel <- newLabel
    addInstruction $ LLVM.Br (Label entryLabel)
    addInstruction $ LLVM.ILabel (Label entryLabel)
    (t1, v1) <- compileExpr e1
    case (t1, v1) of
        (I1, Just val1) -> do
            labelExpr1False <- newLabel
            labelAfter <- newLabel
            addInstruction $ LLVM.BrCond val1 (Label labelAfter) (Label labelExpr1False)
            addInstruction $ LLVM.ILabel (Label labelExpr1False)
            (t2, v2) <- compileExpr e2
            case (t2, v2) of
                (I1, Just val2) -> do
                    addInstruction $ LLVM.Br (Label labelAfter)
                    addInstruction $ LLVM.ILabel (Label labelAfter)
                    n <- newRegister
                    addInstruction $ LLVM.Phi (Register n) I1 [(VConst ConstT, Label entryLabel), (val2, Label labelExpr1False)]
                    return (I1, Just $ VRegister (Register n))
                _ -> throwError $ FrontBug "true || void e2"
        _ -> throwError $ FrontBug "void e1 || e2"

{-
bool b1 = true;
bool b2 = false;
bool b = b1 && b2;

define void @f() #0 {
  %1 = alloca i8, align 1         ;  b1
  %2 = alloca i8, align 1         ;  b2
  %3 = alloca i8, align 1         
  store i8 1, i8* %1, align 1
  store i8 0, i8* %2, align 1

  %4 = load i8, i8* %1, align 1
  %5 = trunc i8 %4 to i1

  br i1 %5, label %6, label %9      ; if %5 jmp %6 else jmp %7

6:                                                ; preds = %0
  %7 = load i8, i8* %2, align 1
  %8 = trunc i8 %7 to i1
  br label %9

9:                                                ; preds = %6, %0
  %10 = phi i1 [ false, %0 ], [ %8, %6 ]

  %11 = zext i1 %10 to i8
  store i8 %11, i8* %3, align 1
  ret void
}



w1 && w2
if not w1 goto Lfalse
if not w2 goto Lfalse
code for Ltrue
Lfalse: 

w1 || w2
if w1 goto Ltrue
if w2 goto Ltrue
code for Lfalse
Ltrue: 
-}

unJustify :: (LLVM.Type, Maybe Value) -> CompilerM(LLVM.Type, Value)
unJustify (t, Just v) = return (t, v)
unJustify (t, Nothing) = throwError $ FrontBug "void exprs called as arguments for function"

call :: Ident -> [(LLVM.Type, Value)] -> CompilerM (LLVM.Type, Maybe Value)
call (Ident ident) args = do
    t <- getFnType ident
    case t of
        LLVM.Void -> do
            addInstruction (Call t ident args Nothing)
            return (t, Nothing)
        _ -> do
            n <- newRegister
            addInstruction (Call t ident args (Just $ Register n))
            return (t, Just $ VRegister (Register n))


indentLns :: [String] -> String
indentLns = concatMap indentLn

indentLn :: String -> String
indentLn line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] then
    line ++ "\n"
  else
    "\t" ++ line ++ "\n"

prologue =  indentLns [
    "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare void @error()",
    "declare i8* @concatStrings(i8*, i8*)",
    "declare i32 @compareStrings(i8*, i8*)",
    "declare i32 @equStrings(i8*, i8*)",
    "declare i32 @neStrings(i8*, i8*)",
    ""]

builtInFuns = [("printInt", LLVM.Void),
    ("printString", LLVM.Void),
    ("readInt", I32),
    ("readString", Ptr I8),
    ("error", LLVM.Void),
    ("concatStrings", Ptr I8),
    ("compareStrings", I32),
    ("equStrings", I32),
    ("neStrings", I32)
    ]

printDebug :: CompilerM ()
printDebug = do
    env <- ask
    state <- get
    throwError $ Debug (show env ++ show state)
{-
builtinsCode = "%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }\n\
\%struct.__sFILEX = type opaque\n\
\%struct.__sbuf = type { i8*, i32 }\n\
\\n\
\@__stdinp = external global %struct.__sFILE*, align 8\n\
\@.str = private unnamed_addr constant [23 x i8] c\"readInt getline error\x0A\00\", align 1\n\
\@.str.1 = private unnamed_addr constant [3 x i8] c\"%d\00\", align 1\n\
\@.str.2 = private unnamed_addr constant [26 x i8] c\"readString getline error\x0A\00\", align 1\n\
\@.str.3 = private unnamed_addr constant [15 x i8] c\"runtime error\x0A\00\", align 1\n\
\\n\
\define i32 @readInt() #0 {\n\
  \%1 = alloca i32, align 4\n\
  \%2 = alloca i8*, align 8\n\
  \%3 = alloca i64, align 8\n\
  \store i8* null, i8** %2, align 8\n\
  \store i64 0, i64* %3, align 8\n\
  \%4 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8\n\
  \%5 = call i64 @getline(i8** %2, i64* %3, %struct.__sFILE* %4)\n\
  \%6 = icmp eq i64 %5, -1\n\
  \br i1 %6, label %7, label %9\n\
\\n\
\7:                                                ; preds = %0\n\
  \%8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str, i64 0, i64 0))\n\
  \call void @exit(i32 1) #3\n\
  \unreachable\n\
\\n\
\9:                                                ; preds = %0\n\
  \%10 = load i8*, i8** %2, align 8\n\
  \%11 = call i32 (i8*, i8*, ...) @sscanf(i8* %10, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i64 0, i64 0), i32* %1)\n\
  \%12 = load i32, i32* %1, align 4\n\
  \ret i32 %12\n\
\}\n\
\\n\
\declare i64 @getline(i8**, i64*, %struct.__sFILE*) #1\n\
\\n\
\; Function Attrs: noreturn\n\
\declare void @exit(i32) #2\n\
\\n\
\declare i32 @sscanf(i8*, i8*, ...) #1\n\
\\n\
\; Function Attrs: noinline nounwind optnone ssp uwtable\n\
\define i8* @readString() #0 {\n\
  \%1 = alloca i8*, align 8\n\
  \%2 = alloca i64, align 8\n\
  \store i8* null, i8** %1, align 8\n\
  \store i64 0, i64* %2, align 8\n\
  \%3 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8\n\
  \%4 = call i64 @getline(i8** %1, i64* %2, %struct.__sFILE* %3)\n\
  \%5 = icmp eq i64 %4, -1\n\
  \br i1 %5, label %6, label %8\n\
\\n\
\6:                                                ; preds = %0\n\
  \%7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.2, i64 0, i64 0))\n\
  \call void @exit(i32 1) #3\n\
  \unreachable\n\
\\n\
\8:                                                ; preds = %0\n\
  \%9 = load i8*, i8** %1, align 8\n\
  \%10 = call i64 @strlen(i8* %9)\n\
  \store i64 %10, i64* %2, align 8\n\
  \%11 = load i8*, i8** %1, align 8\n\
  \%12 = load i64, i64* %2, align 8\n\
  \%13 = sub i64 %12, 1\n\
  \%14 = getelementptr inbounds i8, i8* %11, i64 %13\n\
  \%15 = load i8, i8* %14, align 1\n\
  \%16 = sext i8 %15 to i32\n\
  \%17 = icmp eq i32 %16, 10\n\
  \br i1 %17, label %18, label %23\n\
\\n\
\18:                                               ; preds = %8\n\
  \%19 = load i8*, i8** %1, align 8\n\
  \%20 = load i64, i64* %2, align 8\n\
  \%21 = sub i64 %20, 1\n\
  \%22 = getelementptr inbounds i8, i8* %19, i64 %21\n\
  \store i8 0, i8* %22, align 1\n\
  \br label %23\n\
\\n\
\23:                                               ; preds = %18, %8\n\
  \%24 = load i8*, i8** %1, align 8\n\
  \ret i8* %24\n\
\}\n\
\\n\
\declare i64 @strlen(i8*) #1\n\
\\n\
\define void @error() #0 {\n\
  \%1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.3, i64 0, i64 0))\n\
  \call void @exit(i32 1) #3\n\
  \unreachable\n\
\}\n"

builtins2 = indentLns [
  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
  "",
  "declare i32 @printf(i8*, ...)",
  "",
  "declare i32 @puts(i8*)",
  "",
  "define void @printInt(i32 %x) {",
  "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
  "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
  "ret void",
  "}",
  "define void @printString(i8* %s) {",
  "entry:  call i32 @puts(i8* %s)",
  "ret void",
  "}",
  "",
  "define i32 @main(i32 %argc, i8** %argv) {"]
-}
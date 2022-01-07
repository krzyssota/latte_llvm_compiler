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
import GHC.Exts.Heap (GenClosure(fun))

type Loc = Integer
type Env = M.Map Ident Loc
initialEnv :: M.Map k a
initialEnv = M.empty
type FnEnv = M.Map String LLVM.Type
data CompilerState = CompilerState {
    counter :: Counter,
    instructions :: [Instruction],
    functions :: FnEnv
}
initialCompilerState :: CompilerState
initialCompilerState = CompilerState {
    counter = newCounter,
    instructions = [],
    functions = M.empty
}
type CompilerM = ReaderT Env (ExceptT MyError (StateT CompilerState IO))

newtype Counter = Counter Integer
instance Show Counter where
    show (Counter i) = "Counter " ++ show i
newCounter :: Counter
newCounter = Counter 1
incCounter :: Counter -> Counter
incCounter (Counter i) = Counter $ i+1
castCounter :: Counter -> Integer
castCounter (Counter i) = i
freshInteger :: CompilerM Integer
freshInteger = do
    cs <- get
    let old = castCounter (counter cs) in do
        modify (\s -> s {counter = incCounter $ counter cs})
        return old

addInstruction :: Instruction -> CompilerM ()
addInstruction ins = do
    cs <- get
    modify (\s -> s {instructions = ins:instructions cs})

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
data MyError' a = NotImplemented String | FrontBug String
instance Show MyError where
    show (NotImplemented s) = "NotImplemented " ++ s
    show (FrontBug s) = "FrontBug " ++ s

internalPos :: BNFC'Position
internalPos = Just(-42, -42)

runCompilerM :: CompilerM a -> IO (Either MyError a, CompilerState)
runCompilerM e = runStateT (runExceptT (runReaderT e initialEnv)) initialCompilerState

compile :: Program -> IO ()
compile tree = do
    (err, state) <- runCompilerM (compile' tree)
    case err of
      Left e -> do
          hPutStrLn stderr $ show e
          exitFailure
      Right _ -> putStrLn ( prologue
                    ++ intercalate "\n" (reverse $ map show (instructions state))
                    {-++  "call void @printInt(i32 " ++ showCounter (counter hmm) ++ ")\n" -}
                    ++ epilogue)


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
compileTopDef (FnDef p1 t ident args block) = do
    --throwError $ Test "halko"
    when (ident == Ident "main") (compileBlock block)

compileBlock :: Block -> CompilerM ()
compileBlock (Block p1 stmts) = do
    compileStmts stmts
    return ()

compileStmts :: [Stmt] -> CompilerM ()
compileStmts [] = return ()
compileStmts (s:stmts) = do
    compileStmt s
    compileStmts stmts

compileStmt :: Stmt -> CompilerM ()
compileStmt (Empty p) = return ()
compileStmt (SExp p expr) = do
        (_, _) <- compileExpr expr
        return ()
compileStmt (AbsLatte.Ret p expr) = do
    (t, v) <- compileExpr expr
    case v of
        Nothing -> throwError $ FrontBug "bad ret"
        Just val -> addInstruction (LLVM.Ret t val)

compileStmt _ = throwError $ NotImplemented "compileStmt"

compileExpr :: Expr -> CompilerM (LLVM.Type, Maybe Value)
compileExpr (ELitInt p n) = do
        return (I32, Just (Const n))
compileExpr (EApp p ident exprs) = do
        args_ <- mapM compileExpr exprs
        args <- mapM unJustify args_
        call ident args
compileExpr _ = throwError $ NotImplemented "compileExpr"

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
            n <- freshInteger
            addInstruction (Call t ident args (Just $ Register n))
            return (t, Just $ VRegister (Register n)) -- todo return register in which fn res is stored (Maybe Register has to be added to Call instr)
    -- now we can only call functions for sideeffects


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
    "define i32 @main(i32 %argc, i8** %argv) {"]

epilogue = indentLns [
  "}"]

builtInFuns = [("printInt", LLVM.Void), ("printString", LLVM.Void), ("readInt", I32),("readString", Ptr I8),("error", LLVM.Void)]


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
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleInstances #-}

module CompileToLLVM where
import LLVM
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

type Loc = Int
type Env = M.Map Ident Loc
initialEnv :: M.Map k a
initialEnv = M.empty
data CompilerState = CompilerState {
    counter :: Counter,
    instructions :: [Instruction]
} --M.Map Loc Value
initialCompilerState :: CompilerState
initialCompilerState = CompilerState {
    counter = newCounter,
    instructions = []
}
type CompilerM = ReaderT Env (ExceptT MyError (StateT CompilerState IO))

newtype Counter = Counter Int
newCounter :: Counter
newCounter = Counter 0
incCounter :: Counter -> Counter
incCounter (Counter i) = Counter $ i+1
showCounter :: Counter -> String
showCounter (Counter i) = show i
castCounter :: Counter -> Int
castCounter (Counter i) = i
freshNum :: CompilerM Int
freshNum = do
    cs <- get
    let old = castCounter (counter cs) in do
        modify (\s -> s {counter = incCounter $ counter cs})
        return old
addInstruction :: Instruction -> CompilerM ()
addInstruction ins = do
    cs <- get
    modify (\s -> s {instructions = ins:instructions cs})

type MyError = MyError' BNFC'Position
data MyError' a = NotImplemented String
instance Show MyError where
    show (NotImplemented s) = s

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
      Right _ -> putStrLn (prologue
                    ++ intercalate "\n" (reverse $ map show (instructions state))
                    {-++  "call void @printInt(i32 " ++ showCounter (counter hmm) ++ ")\n" -}
                    ++ epilogue)


compile' :: Program -> CompilerM ()
compile' (Program _ topDefs) = do
    --catchE (mapM_ compileTopDef topDefs) throwE
    mapM_ compileTopDef topDefs

compileTopDef :: TopDef -> CompilerM ()
compileTopDef (FnDef p1 t ident args block) = do
    --throwError $ Test "halko"
    when (ident == Ident "main") (compileBlock block)

compileBlock :: Block -> CompilerM ()
compileBlock (Block p1 stmts) = do
    --addInstruction "call void @printInt(i32 42)"
    --n <- freshNum
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
    addInstruction (LLVM.Ret t v)

compileStmt _ = throwError $ NotImplemented "compileStmt"

compileExpr :: Expr -> CompilerM (LLVM.Type, Value) -- todo maybe value?? to pair with Void
compileExpr (ELitInt p n) = do
        return (I32, Const n)
compileExpr (EApp p ident exprs) = do
        args <- mapM compileExpr exprs
        call ident args
        return (LLVM.Void, Const (-1)) -- todo change
compileExpr _ = throwError $ NotImplemented "compileExpr"

call :: Ident -> [(LLVM.Type, Value)] -> CompilerM ()
call (Ident ident) args = do
    addInstruction (Call LLVM.Void ident args) -- todo get actual fn type


indentLns :: [String] -> String
indentLns = concatMap indentLn

indentLn :: String -> String
indentLn line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] then
    line ++ "\n"
  else
    "\t" ++ line ++ "\n"

prologue = indentLns [
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

epilogue = indentLns [
  "}"]
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleInstances #-}

module CompileToLLVM where
import LLVMDomain as LLVM
import Structures
import SSA
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
import StaticAnalysis (internalPos)

initialCompilerState :: CompilerState
initialCompilerState = CompilerState {
    registerCounter = newCounter,
    labelCounter = newCounter,
    globalsCounter = newCounter,
    globInstructions = [],
    funs = M.empty,
    fnEnv = M.empty,
    currFun = "placeholder"
}

-- type FunsCode = M.Map String (M.Map Label [Instruction])
startNewFun :: String -> Instruction -> CompilerM ()
startNewFun ident def = do
    cs <- get
    let l0 = Label 0
    let bb = newBlock l0
    let newFun = FunCFG {ident = ident, def = def, blocks = M.fromList [(l0, bb)], currLabel = l0, currDefs = M.empty}
    let newFuns = M.insert ident newFun (funs cs)
    modify (\s -> s {funs = newFuns, currFun = ident})

startNewBlock :: Label -> CompilerM()
startNewBlock label = do
    cs <- get
    let currFunIdent = currFun cs
    let currFuns = funs cs
    funCFG <- getCurrFunCFG
    let block = newBlock label
    let newFunCFG = changeCurrLabelInCFG label (addUpdateBlockInCFG label block funCFG)
    modify (\s -> s{funs = M.insert currFunIdent newFunCFG currFuns})

    --modify (\s -> s {currLabel = label, funsCode = (M.insert label [LLVM.ILabel label] funIns):rest})
    --modify (\s -> s {currLabel = label, instructions = LLVM.ILabel label:instructions cs})

addJmp :: Instruction -> CompilerM ()
addJmp (Br l) = do
    --currFunIdent <- gets currFun
    --currBlock <- getCurrBlock
    --addEdge (label currBlock) l currFunIdent
    addInstruction (Br l)
addJmp (BrCond v l1 l2) = do
    --currFunIdent <- gets currFun
    --currBlock <- getCurrBlock
    --addEdge (label currBlock) l1 currFunIdent
    --addEdge (label currBlock) l2 currFunIdent
    addInstruction (BrCond v l1 l2)
addJmp i = throwError $ BackBug ("shouldnt use addJmp for this ins: " ++ show i)

addInstruction :: Instruction -> CompilerM ()
addInstruction (Define a b c) = throwError $ BackBug ("shouldnt use addInstruction for define " ++ show b)
addInstruction DefineMain = throwError $ BackBug ("shouldnt use addInstruction for defineMain ")
addInstruction (ILabel l) = throwError $ BackBug ("shouldnt use addInstruction for new label " ++ show l)
addInstruction (DeclareGString a b c) = throwError $ BackBug ("shouldnt use addInstruction for global string ")
addInstruction ins = do
    cs <- get
    let currFunIdent = currFun cs
    let currFuns = funs cs
    currFunCFG <- getCurrFunCFG
    let label = currLabel currFunCFG
    currBlock <- getCurrBlock
    let newBlock = addInsToBlock ins currBlock
    let newFunCFG = addUpdateBlockInCFG label newBlock currFunCFG
    modify (\s -> s{funs = M.insert currFunIdent newFunCFG currFuns})


   {-  let label = currLabel cs
    let funs = funs cs
    let funIdent = currFun cs
    funCode <- justLookup (M.lookup funIdent funs) ("[AddIns] funIdent in funs" ++ show funIdent ++ show funs)
    currBlock <- justLookup (M.lookup label funCode) ("[AddIns] label in funCode" ++ show label ++ show funCode)
    let newCurrFunCode = M.insert label (currBlock ++ [ins]) funCode
    let newFuns = M.insert funIdent newCurrFunCode funs
    modify (\s -> s{funsCode = newFuns}) -}

   {-  let funIns:rest =  funInstructions cs
    case M.lookup label funIns of
        Just block -> modify (\s -> s {funInstructions = (M.insert label (block++[ins]) funIns):rest})
        Nothing -> throwError $ BackBug ("no instructions for block " ++ show label) -}
    --modify (\s -> s {instructions = ins:instructions cs})

addGlobalInstruction :: Instruction -> CompilerM ()
addGlobalInstruction ins = do
    cs <- get
    modify (\s -> s {globInstructions = ins:globInstructions cs})

getFnType :: String -> CompilerM LLVM.Type
getFnType ident = do
    cs <- get
    let funs = fnEnv cs in
        case M.lookup ident funs of
            Just t -> return t
            Nothing -> throwError $ FrontBug ("no such ident " ++ ident)


runCompilerM :: CompilerM a -> IO (Either MyError a, CompilerState)
runCompilerM e = runStateT (runExceptT (runReaderT e M.empty)) initialCompilerState

compile :: Program -> IO ()
compile tree = do
    (err, state) <- runCompilerM (compile' tree)
    case err of
      Left e -> do
          putStrLn ( "managed to compile before error\n"
            ++ showDebug state ++ "\n\n"
            ++ intercalate "\n" (map show (globInstructions state))
            ++ "\n" ++ prologue
            ++ showCompiledCode state)
          hPrint stderr e
          exitFailure
      Right _ -> putStrLn (
                    intercalate "\n" (map show (globInstructions state))
                    ++ "\n" ++ prologue
                    ++ showCompiledCode state)


compile' :: Program -> CompilerM ()
compile' (Program _ topDefs) = do
    mapM_ registerBuiltInFn builtInFuns
    mapM_ registerFn topDefs
    mapM_ compileTopDef topDefs
    convertToSSA 

compileTopDef :: TopDef -> CompilerM ()
compileTopDef (FnDef p1 t (Ident ident) args (Block p2 stmts)) = do
    funArgs <- mapM (funArg . llvmArg) args
    case ident of
        "main" -> do
            startNewFun "main" DefineMain
            compileStmts stmts
            return ()
        _ -> do
            startNewFun ident (Define (llvmType t) ident (map funDefArg funArgs))
            env <- declareFunArgs funArgs
            local (const env) $ compileStmts stmts
            when (llvmType t == LLVM.Void) (addInstruction RetVoid)

registerBuiltInFn :: (String, LLVM.Type) -> CompilerM ()
registerBuiltInFn (ident, t) = do
    cs <- get
    modify (\s -> s {fnEnv = M.insert ident t (fnEnv cs)})

registerFn :: TopDef -> CompilerM ()
registerFn (FnDef p1 t (Ident ident) args _) = do
    cs <- get
    modify (\s -> s {fnEnv = M.insert ident (LLVM.llvmType t) (fnEnv cs)})

funDefArg :: (LLVM.Type, Ident, Register) -> (LLVM.Type, Register)
funDefArg (t, _, r) = (t, r)

funArg :: (LLVM.Type, Ident) -> CompilerM (LLVM.Type, Ident, Register)
funArg (t, ident) = do
    r <- newRegister
    return (t, ident, r)

declareFunArg :: (LLVM.Type, Ident, Register) -> CompilerM VarsEnv
declareFunArg (t, ident, r1) = do
    r2 <- newRegister
    addInstruction (LLVM.Alloc r2 t)
    addInstruction (LLVM.Store (VRegister r1 t) t r2)
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
            addInstruction (LLVM.Ret val)
            env <- ask
            return (env, True)
compileStmt (AbsLatte.VRet p) = do
    addInstruction LLVM.RetVoid
    env <- ask
    return (env, True)
compileStmt (Decl p t (it:items)) = do
    env <- declareVar t it
    local (const env) $ compileStmt (Decl p t items)
compileStmt (Decl p t []) = defaultRes
compileStmt (BStmt p1 (Block p2 stmts)) = do
    env <- ask
    (_, ret) <- local (const env) (compileStmts stmts)
    return (env, ret)
compileStmt (Ass p1 ident expr) = do
    (t, v) <- compileExpr expr
    (declaredT, r) <- getVar ident
    when (t /= declaredT) (throwError $ FrontBug "ass with wrong type")
    case v of
        Just val -> do
            addInstruction (LLVM.Store val t r)
            defaultRes
        Nothing -> throwError $ FrontBug "assigning void expr to var"
compileStmt (Incr _ ident) = do
    r1 <- newRegister
    r2 <- newRegister
    (t, r) <- getVar ident
    addInstruction $ LLVM.Load r1 t r
    addInstruction $ LLVM.Ari r2 LLVM.Add (VRegister r1 t) (VConst (ConstI 1))
    addInstruction $ LLVM.Store (VRegister r2 I32) t r

    defaultRes
compileStmt (Decr _ ident) = do
    r1 <- newRegister
    r2 <- newRegister
    (t, r) <- getVar ident
    addInstruction $ LLVM.Load r1 t r
    addInstruction $ LLVM.Ari r2 LLVM.Add (VRegister r1 t) (VConst (ConstI (-1)))
    addInstruction $ LLVM.Store (VRegister r2 I32) t r
    defaultRes
compileStmt (Cond _ expr stmt) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just (VConst ConstT)) -> do
            compileStmt stmt
        (I1, Just (VConst ConstF)) -> do
            defaultRes
        (I1, Just val) -> do
            thenLabel <- newLabel
            afterLabel <- newLabel
            addJmp $ LLVM.BrCond val thenLabel afterLabel
            startNewBlock thenLabel
            (_, ret) <- compileStmt stmt
            unless ret (addJmp $ LLVM.Br afterLabel)
            startNewBlock afterLabel
            defaultRes
        _ -> throwError $ FrontBug "not bool cond in if"

compileStmt (CondElse _ expr s1 s2) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just (VConst ConstT)) -> do
            compileStmt s1
        (I1, Just (VConst ConstF)) -> do
            compileStmt s2
        (I1, Just val) -> do
            thenLabel <- newLabel
            elseLabel <- newLabel
            addJmp $ LLVM.BrCond val thenLabel elseLabel
            startNewBlock thenLabel
            (_, ret1) <- compileStmt s1
            afterLabel <- newLabel
            unless ret1 (addJmp $ LLVM.Br afterLabel)
            startNewBlock elseLabel
            (_, ret2) <- compileStmt s2
            unless ret2 (addJmp $ LLVM.Br afterLabel)
            when (not ret1 || not ret2)$ startNewBlock afterLabel -- TODO tutaj ew bug
            env <- ask
            return (env, ret1 && ret2)
        _ -> throwError $ FrontBug "not bool cond in ifelse"
compileStmt (While _ expr stmt) = do
    checkCondLabel <- newLabel
    addJmp $ LLVM.Br checkCondLabel
    bodyLabel <- newLabel
    startNewBlock bodyLabel
    (_, ret) <- compileStmt stmt
    addJmp $ LLVM.Br checkCondLabel
    startNewBlock checkCondLabel
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            afterLabel <- newLabel
            addJmp $ LLVM.BrCond val bodyLabel afterLabel
            startNewBlock afterLabel
            defaultRes
        _ -> throwError $ FrontBug "not bool cond in while"

getVar :: Ident -> CompilerM (LLVM.Type, Register)
getVar ident = do
    env <- ask
    case M.lookup ident env of
        Just (t, r) -> return (t, r)
        Nothing -> throwError $ FrontBug "get undeclared bar"

declareVar :: AbsLatte.Type -> Item -> CompilerM VarsEnv
declareVar t item = do
    r <- newRegister
    addInstruction (LLVM.Alloc r (llvmType t))
    case item of
        Init p ident expr -> do
            addToVarsDeclared ident
            (t2, v) <- compileExpr expr
            when (llvmType t /= t2) (throwError $ FrontBug "decl mismatched types")
            case v of
                Just v -> do
                    addInstruction (LLVM.Store v t2 r)
                    asks $ M.insert ident (llvmType t, r)
                Nothing -> throwError $ FrontBug "void item in decl"

        NoInit p ident -> do
            addToVarsDeclared ident
            when (llvmType t == I32) $ addInstruction (LLVM.Store (VConst (ConstI 0)) (llvmType t) r)
            when (llvmType t == I1) $ addInstruction (LLVM.Store (VConst ConstF) (llvmType t) r)
            asks $ M.insert ident (llvmType t, r)

compileExpr :: Expr -> CompilerM (LLVM.Type, Maybe Value)
compileExpr (ELitInt p i) =
        return (I32, Just (VConst (ConstI (fromIntegral i))))
compileExpr (EApp p ident exprs) = do
        args_ <- mapM compileExpr exprs
        args <- mapM unJustify args_
        call ident (map snd args)
compileExpr (ELitTrue p) =
    return (I1, Just (VConst ConstT))
compileExpr (ELitFalse p) =
    return (I1, Just (VConst ConstF))
compileExpr (EVar p ident) = do
    r1 <- newRegister
    env <- ask
    case M.lookup ident env of
        Just (t, r2) -> do
            addInstruction (Load r1 t r2)
            return (t, Just $ VRegister r1 t) -- TODO opt return only Value
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
                addInstruction $ LLVM.Ari r (llvmAddOp addOp) val1 val2
                return (t1, Just $ VRegister r t1)
        (Ptr I8, Just val1, Ptr I8, Just val2) ->
                call (Ident "__concatStrings__") [val1, val2]
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (EMul p e1 mulOp e2) = do
    (t1, v1) <- compileExpr e1
    (t2, v2) <- compileExpr e2
    case (t1, v1, t2, v2) of
        (_, Nothing, _, _) -> throwError $ FrontBug "ari op with void expr"
        (_, _, _, Nothing) -> throwError $ FrontBug "ari op with void expr"
        (I32, Just val1, I32, Just val2) -> do
                r <- newRegister
                addInstruction $ LLVM.Ari r (llvmMulOp mulOp) val1 val2
                return (t1, Just $ VRegister r t1)
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (ERel _ e1 relOp e2) = do
    (t1, v1) <- compileExpr e1
    (t2, v2) <- compileExpr e2
    case (t1, v1, t2, v2) of
        (_, Nothing, _, _) -> throwError $ FrontBug "ari op with void expr"
        (_, _, _, Nothing) -> throwError $ FrontBug "ari op with void expr"
        (I32, Just val1, I32, Just val2) -> do
                r <- newRegister
                addInstruction $ LLVM.Cmp r (llvmRelOp relOp) I32 val1 val2
                return (I1, Just $ VRegister r I1)
        (I1, Just val1, I1, Just val2) -> do
            case relOp of
                EQU _ -> do
                    r <- newRegister
                    addInstruction $ LLVM.Cmp r (llvmRelOp relOp) I1 val1 val2
                    return (I1, Just $ VRegister r I1)
                AbsLatte.NE _ -> do
                    r <- newRegister
                    addInstruction $ LLVM.Cmp r (llvmRelOp relOp) I1 val1 val2
                    return (I1, Just $ VRegister r I1)
                _ -> throwError $ FrontBug "string relOp other than EQU NE"
        (Ptr I8, Just val1, Ptr I8, Just val2) -> do
            case relOp of
                EQU _ -> do
                    (_, v) <- call (Ident "__equStrings__") [val1, val2]
                    return (I1, v)
                AbsLatte.NE _ -> do
                    (_, v) <- call (Ident "__neStrings__") [val1, val2]
                    return (I1, v)
                _ -> throwError $ FrontBug "string relOp other than EQU NE"
        _ -> throwError $FrontBug "ari op with wrong type"
compileExpr (Neg _ expr) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I32, Just val) -> do
            r <- newRegister
            addInstruction $ LLVM.Ari r LLVM.Sub (VConst (ConstI 0)) val
            return (I32, Just $ VRegister r I32)
        (_, Just val) -> throwError $ FrontBug "neg of not i32 type"
        (_, Nothing) -> throwError $ FrontBug "neg of void expr"
compileExpr (Not _ expr) = do
    (t, v) <- compileExpr expr
    case (t, v) of
        (I1, Just val) -> do
            r <- newRegister
            addInstruction $ LLVM.Xor r val (VConst ConstT)
            return (I1, Just $ VRegister r I1)
        (_, Just val) -> throwError $ FrontBug "not of not i1 type"
        (_, Nothing) -> throwError $ FrontBug "not of void expr"
compileExpr (EAnd _ e1 e2) = do
    entryLabel <- newLabel
    addJmp $ LLVM.Br entryLabel
    startNewBlock entryLabel
    (t1, v1) <- compileExpr e1
    e1Label <- getCurrLabel
    case (t1, v1) of
        (I1, Just val1) -> do
            labelExpr1True <- newLabel
            labelAfter <- newLabel
            addJmp $ LLVM.BrCond val1 labelExpr1True labelAfter
            startNewBlock labelExpr1True
            (t2, v2) <- compileExpr e2
            e2Label <- getCurrLabel
            case (t2, v2) of
                (I1, Just val2) -> do
                    addJmp $ LLVM.Br labelAfter
                    startNewBlock labelAfter
                    r <- newRegister
                    addInstruction $ LLVM.IPhi r I1 [(VConst ConstF, e1Label), (val2, e2Label)]
                    return (I1, Just $ VRegister r I1)
                _ -> throwError $ FrontBug "true %% void e2"
        _ -> throwError $ FrontBug "void e1 %% e2"
compileExpr (EOr _ e1 e2) = do
    entryLabel <- newLabel
    addJmp $ LLVM.Br entryLabel
    startNewBlock entryLabel
    (t1, v1) <- compileExpr e1
    e1Label <- getCurrLabel
    case (t1, v1) of
        (I1, Just val1) -> do
            labelExpr1False <- newLabel
            labelAfter <- newLabel
            addJmp $ LLVM.BrCond val1 labelAfter labelExpr1False
            startNewBlock labelExpr1False
            (t2, v2) <- compileExpr e2
            e2Label <- getCurrLabel
            case (t2, v2) of
                (I1, Just val2) -> do
                    addJmp $ LLVM.Br labelAfter
                    startNewBlock labelAfter
                    r <- newRegister
                    addInstruction $ LLVM.IPhi r I1 [(VConst ConstT, e1Label), (val2, e2Label)]
                    return (I1, Just $ VRegister r I1)
                _ -> throwError $ FrontBug "true || void e2"
        _ -> throwError $ FrontBug "void e1 || e2"

unJustify :: (LLVM.Type, Maybe Value) -> CompilerM(LLVM.Type, Value)
unJustify (t, Just v) = return (t, v)
unJustify (t, Nothing) = throwError $ FrontBug "void exprs called as arguments for function"

call :: Ident -> [Value] -> CompilerM (LLVM.Type, Maybe Value)
call (Ident ident) args = do
    t <- getFnType ident
    case t of
        LLVM.Void -> do
            addInstruction (Call Nothing t ident args)
            return (t, Nothing)
        _ -> do
            r <- newRegister
            addInstruction (Call (Just r) t ident args )
            return (t, Just $ VRegister r t)


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
    "declare i8* @__concatStrings__(i8*, i8*)",
    "declare i32 @__equStrings__(i8*, i8*)",
    "declare i32 @__neStrings__(i8*, i8*)",
    ""]

builtInFuns = [("printInt", LLVM.Void),
    ("printString", LLVM.Void),
    ("readInt", I32),
    ("readString", Ptr I8),
    ("error", LLVM.Void),
    ("__concatStrings__", Ptr I8),
    ("__equStrings__", I32),
    ("__neStrings__", I32)
    ]

printDebug :: CompilerM ()
printDebug = do
    env <- ask
    state <- get
    throwError $ Debug (show env ++ show state)
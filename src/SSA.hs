{-# LANGUAGE FlexibleInstances #-}

module SSA where
import qualified AbsLatte
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Structures
import LLVMDomain
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe

convertToSSA :: CompilerM ()
convertToSSA = do
    cs <- get
    liftIO $ putStrLn ( "before ssa\n"
            ++ showCompiledCode cs)
    mapM_ transFun (funs cs)
    cs <- get
    mainFun <- justLookup "main" (funs cs) "convertToSSA"
    liftIO $ putStrLn ( "after ssa\n"
            ++ "main currDefs " ++ show (currDefs mainFun) ++ "\n"
            ++ showCompiledCode cs)

transFun :: FunCFG -> CompilerM ()
transFun fun = do
    modify (\s -> s{currFun = ident fun})
    addEdgesForFun fun
    mapM_ (transBlock . label) (M.elems $ blocks fun)

transBlock :: Label -> CompilerM()
transBlock l = do
    block <- getBlockFromCurrFun l
    newInss <- transInss l (inss block) []
    newBlock <- getBlockFromCurrFun l
    let phs = getInsFromPhis newBlock
    let finalBlock = newBlock{inss = phs ++ newInss}
    modifyBlockInCurrFun finalBlock

transInss :: Label -> [Instruction] -> [Instruction] -> CompilerM [Instruction]
transInss l [] inssOut = return $ reverse inssOut
transInss l (ins:inssIn) inssOut = do
    newIns <- transIns l ins
    transInss l inssIn (newIns:inssOut)

transIns :: Label -> Instruction -> CompilerM Instruction
transIns label ins =
    case ins of
        BrCond v l1 l2 -> do
            v' <- readVal label v
            return $ BrCond v' l1 l2
        Ret v -> do
            v' <- readVal label v
            return $ Ret v'
        Alloc _ _ -> return Nop
        Store v _ r -> do -- store value v in register r
            v' <- readVal label v
            writeVariable label r v'
            return Nop
        Load r1 t r2 -> do
            v <- readVal label (VRegister r2 t)
            writeVariable label r1 v
            return Nop
        DeclareGString l s i -> return $ DeclareGString l s i
        Ari r op v1 v2 -> do
            v1' <- readVal label v1
            v2' <- readVal label v2
            writeVariable label r (VRegister r I32) -- TODO check if necessary
            return $ Ari r op v1' v2'
        Xor r v1 v2 -> do
            v1' <- readVal label v1
            v2' <- readVal label v2
            writeVariable label r (VRegister r I32) -- TODO check if necessary
            return $ Xor r v1' v2'
        Cmp r op valsType v1 v2 -> do
            v1' <- readVal label v1
            v2' <- readVal label v2
            writeVariable label r (VRegister r I1)
            return $ Cmp r op valsType v1' v2'
        Call mr retType ident args -> do
            args' <- mapM (readVal label) args
            when (isJust mr) (writeVariable label (fromJust mr) (VRegister (fromJust mr) retType))
            return $  Call mr retType ident args'
        _ -> return ins -- Define, DefineMain, Nop, Br, RetVoid, ILabel, Phi (TODO czy na pewno Phi tez?)

    --throwError $ Debug ("transIns not implemented for " ++ show x)

readVal :: Label -> Value -> CompilerM Value
readVal l (VRegister r t) = readVariable l r t
readVal l v = return v

readVariable :: Label -> Register -> Type -> CompilerM Value
readVariable l r t = do
    cs <- get
    let funIdent = currFun cs
    fun <- justLookup funIdent (funs cs) "readVariable"
    case M.lookup (r, l) (currDefs fun) of
        Just val -> return val
        Nothing -> readVariableRecursively l r t

readVariableRecursively :: Label -> Register -> Type -> CompilerM Value
readVariableRecursively l variable t = do
    block <- getBlockFromCurrFun l
    if S.size (preds block) == 1
        then do
            val <- readVariable (head (S.toList (preds block))) variable t
            writeVariable l variable val
            return val
        else do
            r <- newRegister
            putNewEmptyPhi l r t
            let newVal = VRegister r t
            writeVariable l variable newVal
            return newVal

putNewEmptyPhi :: Label -> Register -> Type -> CompilerM ()
putNewEmptyPhi l r t = do
    block <- getBlockFromCurrFun l
    --liftIO $ putStrLn ("before\n" ++ showBlock block)
    let newBlock = block{phis = M.insert r (t, []) (phis block)}
    modifyBlockInCurrFun newBlock
    --block <- getBlockFromCurrFun l
    --liftIO $ putStrLn ("after\n" ++ showBlock block ++ "\n\n")


writeVariable :: Label -> Register -> Value -> CompilerM ()
writeVariable l r v = do
    fun <- getCurrFunCFG
    let newCurrDefs = M.insert (r, l) v (currDefs fun)
    let newFun = fun{currDefs = newCurrDefs}
    modify (\s -> s{funs = M.insert (ident newFun) newFun (funs s)})




addEdgesForFun:: FunCFG -> CompilerM ()
addEdgesForFun fun = mapM_ (addEdgesForBlock (ident fun)) (M.elems $ blocks fun)

addEdgesForBlock :: String -> BasicBlock -> CompilerM ()
addEdgesForBlock funIdent b = do
    mapM_ (addEdgesForInstruction funIdent b) (inss b)

addEdgesForInstruction :: String -> BasicBlock -> Instruction -> CompilerM ()
addEdgesForInstruction funIdent b (Br l) = do
    addEdge (label b) l funIdent
addEdgesForInstruction funIdent b (BrCond _ l1 l2) = do
    addEdge (label b) l1 funIdent
    addEdge (label b) l2 funIdent
addEdgesForInstruction _ _ _ = return ()




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

convertToSSA :: CompilerM ()
convertToSSA = do
    cs <- get
    mapM_ transFun (funs cs)

transFun :: FunCFG -> CompilerM ()
transFun fun = do
    modify (\s -> s{currFun = ident fun})
    addEdgesForFun fun
    mapM_ transBlock (M.elems $ blocks fun)

transBlock :: BasicBlock -> CompilerM()
transBlock b = do
    liftIO $ print (showDebugBlock b)
    mapM_ (transIns (label b)) (inss b)

transInss :: Label -> [Instruction] -> [Instruction] -> CompilerM [Instruction]
transInss l [] inssOut = return $ reverse inssOut
transInss l (ins:inssIn) inssOut = do
    newIns <- transIns l ins
    transInss l inssIn (newIns:inssOut)

transIns :: Label -> Instruction -> CompilerM Instruction
transIns _ (Br l) = return $ Br l
transIns label (BrCond v l1 l2) = do
    v' <- readVal label v
    return $ BrCond v' l1 l2
transIns l x = do
    return x
    throwError $ Debug ("transIns not implemented for " ++ show x)

readVal :: Label -> Value -> CompilerM Value
readVal l (VRegister r) = readVariable l r
readVal l v = return v

readVariable :: Label -> Register -> CompilerM Value
readVariable l r = do
    cs <- get
    let funIdent = currFun cs
    fun <- justLookup funIdent (funs cs) "readVariable"
    case M.lookup (r, l) (currDefs fun) of
        Just val -> return val
        Nothing -> readVariableRecursively l r

readVariableRecursively :: Label -> Register -> CompilerM Value
readVariableRecursively l currRegForVar = do
    block <- getBlockFromCurrFun l
    if S.size (preds block) == 1
        then readVariable (head (S.toList (preds block))) currRegForVar
        else do
            newRegForVar <- newRegister
            putNewEmptyPhi l newRegForVar
            writeVariable l currRegForVar newRegForVar
            return (VRegister newRegForVar)

    throwError $ Debug ""

putNewEmptyPhi :: Label -> Register -> CompilerM ()
putNewEmptyPhi l r = throwError $ NotImplemented "putNewEmptyPhi"

writeVariable :: Label -> Register -> Value -> CompilerM ()
writeVariable l r v = do
    fun <- getCurrFunCFG
    let newCurrDefs = M.insert (r, l) v (currDefs fun) 
    let newFun = fun{currDefs = newCurrDefs}
    modify (\s -> s{funs = M.insert (ident newFun) fun (funs s)})



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


    

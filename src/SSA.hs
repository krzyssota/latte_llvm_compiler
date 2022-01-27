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
    {- liftIO $ putStrLn ( "\n\nbefore ssa\n"
            ++ showCompiledCode cs) -}
    mapM_ addEdgesForFun (funs cs)
    cs1 <- get
    mapM_ transFun (funs cs1)
    cs2 <- get
    mapM_ transAddPhis (funs cs2)
    {- cs3 <- get
    liftIO $ putStrLn ( "\n\nwith phis\n"
            ++ showCompiledCode cs3) -}

-- removeTrivialPhis
-- %A = phi i32 [(%B, l_0), (%B, l_1)] ->
    -- %A = %B ->
        -- kazde uzycie %A zamienić na %B (%C = %D + %A -> %C = %D + %B) instrukcja %C = %D + %A  jest userem tego phi. trzeba to zebrac przechodzac kwadrotowo po kodzie

transFun :: FunCFG -> CompilerM ()
transFun fun = do
    modify (\s -> s{currFun = ident fun})
    cs <- get 
    let blocks' = map label $ M.elems $ blocks fun -- lex block order
    blocks' <- bfsOrder fun     -- bfs of CFG order
    transIns (Label 0) (def fun)
    mapM_ transBlock blocks'
        where
            transBlock :: Label -> CompilerM()
            transBlock l = do
                block <- getBlockFromCurrFun l
                newInss <- transInss l (inss block) []
                newBlock <- getBlockFromCurrFun l
                let finalBlock = newBlock{inss = newInss}
                modifyBlockInCurrFun finalBlock
                where
                    transInss :: Label -> [Instruction] -> [Instruction] -> CompilerM [Instruction]
                    transInss l [] inssOut = return $ reverse inssOut
                    transInss l (ins:inssIn) inssOut = do
                        maybeNewIns <- transIns l ins
                        cs <- get
                        {- liftIO $ putStrLn (show l ++ show ins ++ " -> " ++ show maybeNewIns
                            ++ "\ncurrDefs:\n" ++ showCurrentDefs cs
                            ++ "\n") -}
                        if isJust maybeNewIns
                            then transInss l inssIn (fromJust maybeNewIns:inssOut)
                            else transInss l inssIn inssOut

            bfsOrder :: FunCFG -> CompilerM [Label]
            bfsOrder fun = do
                entryBlock <- justLookup (Label 0) (blocks fun) ("[bfsOrder]")
                bfs fun (S.toList $ succs entryBlock) S.empty [Label 0]
                where
                    bfs :: FunCFG -> [Label] -> S.Set Label -> [Label] -> CompilerM [Label]
                    bfs _ [] _ lOut = return lOut
                    bfs fun (l:queue) seen lOut = do
                        if S.member l seen
                            then do
                                bfs fun queue seen lOut
                            else do
                                block <- justLookup l (blocks fun) ("[bfs] " ++ show l)
                                -- difference: Return elements of the first set not existing in the second set.
                                bfs fun (queue ++ S.toList (succs block)) (S.insert l seen) (lOut ++ [l])

transIns :: Label -> Instruction -> CompilerM (Maybe Instruction)
transIns label ins =
    case ins of
        BrCond v l1 l2 -> do
            v' <- readValue label v
            return (Just $ BrCond v' l1 l2)
        Ret v -> do
            v' <- readValue label v
            return (Just $ Ret v')
        Alloc _ _ -> return Nothing
        Store v _ r -> do -- store value v in register r
            v' <- readValue label v
            writeVariable label r v'
            return Nothing
        Load r1 t r2 -> do
            v <- readValue label (VRegister r2 t)
            writeVariable label r1 v
            return Nothing
        DeclareGString l s i -> return (Just $ DeclareGString l s i)
        Ari r op v1 v2 -> do
            v1' <- readValue label v1
            v2' <- readValue label v2
            writeVariable label r (VRegister r I32) -- TODO check if necessary
            return (Just $ Ari r op v1' v2')
        Xor r v1 v2 -> do
            v1' <- readValue label v1
            v2' <- readValue label v2
            writeVariable label r (VRegister r I32) -- TODO check if necessary
            return (Just $ Xor r v1' v2')
        Cmp r op valsType v1 v2 -> do
            v1' <- readValue label v1
            v2' <- readValue label v2
            writeVariable label r (VRegister r I1)
            return (Just $ Cmp r op valsType v1' v2')
        Call mr retType ident args -> do
            args' <- mapM (readValue label) args
            when (isJust mr) (writeVariable label (fromJust mr) (VRegister (fromJust mr) retType))
            return (Just $ Call mr retType ident args')
        IPhi r t entries -> do
            writeVariable label r (VRegister r t) -- TODO hmm czy to potrzebne

            {- cs <- get
            liftIO $ putStrLn ("currDefs before putPhi: " ++ showCurrentDefs cs) -}
            --entries' <- mapM (readEntryVal label) entries
            --putPhi label ((r, (t, entries')), Nothing)
            --return Nothing
            return $ Just (IPhi r t entries)
        Define retType ident args -> do
            mapM_ (\(t, r) -> writeVariable label r (VRegister r t)) args
            return $ Just $ Define retType ident args
        _ -> return $ Just ins -- DefineMain, Br, RetVoid, ILabel

transAddPhis :: FunCFG -> CompilerM ()
transAddPhis fun = do
    modify (\s -> s{currFun = ident fun})
    mapM_ (transAddPhisInBlock . label) (M.elems $ blocks fun)
    where
        transAddPhisInBlock :: Label -> CompilerM()
        transAddPhisInBlock l = do
            block <- getBlockFromCurrFun l

            {- when (phis block /= []) (do
                    cs <- get
                    liftIO $ putStrLn ("phis1 for block: " ++ show l
                        ++ "-> " ++ show (phis block)
                        ++ "\ncurrDefs: " ++ showCurrentDefs cs)) -}

            let nonEmptyPhis = map fst (filter (\(_, variable) -> isNothing variable) $ phis block)  -- TODO should probably do this
            nonEmptyPhis' <- mapM (rereadEntries l) nonEmptyPhis

            let emptyPhis = filter (\(_, variable) -> isJust variable) $ phis block
            filledPhis <- mapM (addPhiOperands l . (\(p, v) -> (p, fromJust v))) emptyPhis

            --let newPhis = nonEmptyPhis' ++ filledPhis
            let newPhis = filledPhis

           {-  when (newPhis /= []) (do
                    cs <- get
                    liftIO $ putStrLn ("phis2 for block: " ++ show l ++ "-> " ++ show newPhis
                        ++ "\ncurrDefs: " ++ showCurrentDefs cs)) -}

            block <- getBlockFromCurrFun l
            let finalBlock = block{inss = map getInsFromPhi newPhis ++ inss block, phis = []}
            modifyBlockInCurrFun finalBlock
                where
                    rereadEntries :: Label -> Phi -> CompilerM Phi
                    rereadEntries l (r, (t, entries)) = do
                        entries' <- mapM (readEntryVal l) entries
                        return (r, (t, entries'))

putPhi :: Label -> (Phi, Maybe Register) -> CompilerM (Phi, Maybe Register)
putPhi l (p, mr) = do
    block <- getBlockFromCurrFun l
    let newBlock = block{phis = (p, mr):phis block}
    --liftIO $ putStrLn (show l ++ ": put phi: " ++ show p ++ " " ++ show mr)
    modifyBlockInCurrFun newBlock
    return (p, mr)

addPhiOperands :: Label -> (Phi, Register) -> CompilerM Phi
addPhiOperands l (phi, variable) = do
    --liftIO $ print ("addPhiOperands " ++ show phi ++ " " ++ show variable)
    block <- getBlockFromCurrFun l
    addPhiOperands' l phi variable (S.toList $ preds block)
    where
        addPhiOperands' :: Label -> Phi -> Register -> [Label] -> CompilerM Phi
        addPhiOperands' l phi variable [] = return phi
        addPhiOperands' l (r, (t, entries)) variable (predLabel:preds) = do
            v <- readVariable predLabel variable t
            addPhiOperands' l (r, (t, (v, predLabel):entries)) variable preds

readEntryVal :: Label -> (Value, Label) -> CompilerM (Value, Label)
readEntryVal label (v, l) = do
    v' <- readValue l v -- sic! l not label 
    return (v', l)

readValue :: Label -> Value -> CompilerM Value
readValue l (VRegister variable t) = readVariable l variable t
readValue l v = return v

readVariable :: Label -> Register -> Type -> CompilerM Value
readVariable l variable t = do
    --liftIO $putStrLn ("readVariable " ++ show l ++ " v: " ++ show variable)
    cs <- get
    let funIdent = currFun cs
    fun <- justLookup funIdent (funs cs) "readVariable"
    case M.lookup (l, variable) (currDefs fun) of
        Just val -> return val
        Nothing -> readVariableRecursively l variable t

readVariableRecursively :: Label -> Register -> Type -> CompilerM Value
readVariableRecursively l variable t = do
    block <- getBlockFromCurrFun l
    val <- if S.size (preds block) == 1
        then do
            --liftIO $putStrLn ("readVariableRecursively " ++ show l ++ " v: " ++ show variable ++ " single pred")
            val <- readVariable (head (S.toList (preds block))) variable t
            writeVariable l variable val
            return val
        else do
            r <- newRegister
            --liftIO $putStrLn ("readVariableRecursively " ++ show l ++ " v: " ++ show variable ++ " puttinh empty phi " ++ show r )
            (emptyPhi, r2) <- putNewEmptyPhi l r t variable
            let newVal = VRegister r t
            writeVariable l variable newVal -- end recursion
            addPhiOperands l (emptyPhi, fromJust r2)
            return newVal
    --liftIO $putStrLn ("readVariableRecursively " ++ show l ++ " v: " ++ show variable ++ " returning " ++ show val )
    return val
    where
        putNewEmptyPhi :: Label -> Register -> Type -> Register -> CompilerM (Phi, Maybe Register)
        putNewEmptyPhi l r t variable = putPhi l ((r, (t, [])), Just variable)

writeVariable :: Label -> Register -> Value -> CompilerM ()
writeVariable l r v = do
    --liftIO $putStrLn ("writeVariable " ++ show l ++ " " ++ show r ++ " -> " ++ show v)
    fun <- getCurrFunCFG
    let newCurrDefs = M.insert (l, r) v (currDefs fun)
    let newFun = fun{currDefs = newCurrDefs}
    modify (\s -> s{funs = M.insert (ident newFun) newFun (funs s)})


addEdgesForFun:: FunCFG -> CompilerM ()
addEdgesForFun fun = mapM_ (addEdgesForBlock (ident fun)) (M.elems $ blocks fun)
                where
                    addEdgesForBlock :: String -> BasicBlock -> CompilerM ()
                    addEdgesForBlock funIdent b =
                        mapM_ (addEdgesForInstruction funIdent b) (inss b)

                    addEdgesForInstruction :: String -> BasicBlock -> Instruction -> CompilerM ()
                    addEdgesForInstruction funIdent b (Br l) =
                        addEdge (label b) l funIdent
                    addEdgesForInstruction funIdent b (BrCond _ l1 l2) = do
                        addEdge (label b) l1 funIdent
                        addEdge (label b) l2 funIdent
                    addEdgesForInstruction _ _ _ = return ()



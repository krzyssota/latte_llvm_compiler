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
import Data.List

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
    calcDomTree
    cs3 <- get
    {- liftIO $ putStrLn ( "\n\nbefore triv phi\n"
            ++ 
            showCompiledCode cs3) -}
    mapM_ removeTrivPhi (funs cs3)
{-     liftIO $ putStrLn ( "\n\nbefore gcse\n"
            ++ 
            showCompiledCode cs3) -}
    cs4 <- get
    mapM_ gcse (funs cs4)
    cs5 <- get
    {- liftIO $ putStrLn ( "\n\nbefore triv phi\n"
            ++ 
            showCompiledCode cs3) -}
    mapM_ removeTrivPhi (funs cs5)
    

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
            nonEmptyPhis' <- mapM (rereadEntriesInPhi l) nonEmptyPhis

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

rereadEntriesInPhi :: Label -> Phi -> CompilerM Phi
rereadEntriesInPhi l (r, (t, entries)) = do
    entries' <- mapM (readEntryVal l) entries
    return (r, (t, entries'))

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


calcDomTree :: CompilerM ()
calcDomTree = do
    cs <- get
    mapM_ prepDomInFun (M.elems $ funs cs)
    cs <- get
    mapM_ calcDomInFun (M.elems $ funs cs)
    where
        prepDomInFun :: FunCFG -> CompilerM ()
        prepDomInFun fun = do
            let fun1 = addFullDomsInBlocks fun (M.elems $ blocks fun)
            fun2 <- addSingleDomToEntryBlock fun1
            modify(\s -> s{funs = M.insert (ident fun2) fun2 (funs s)})
            where
                addFullDomsInBlocks :: FunCFG -> [BasicBlock] -> FunCFG
                addFullDomsInBlocks fun [] = fun
                addFullDomsInBlocks fun (b:bs) =
                    let newFun = addFullDomsInBlock fun b in
                    addFullDomsInBlocks newFun bs

                addFullDomsInBlock :: FunCFG -> BasicBlock -> FunCFG
                addFullDomsInBlock fun b =
                    let newBlock = b{domPreds = S.fromList $ M.keys (blocks fun)} in
                    addUpdateBlockInCFG (label newBlock) newBlock fun

                addSingleDomToEntryBlock :: FunCFG -> CompilerM FunCFG
                addSingleDomToEntryBlock fun = do
                    entryBlock <- justLookup (Label 0) (blocks fun) ("[addSingleDomToEntryBlock] Label 0 " ++ show fun)
                    let newEntryBlock = entryBlock{domPreds = S.singleton (Label 0)}
                    return $ addUpdateBlockInCFG (Label 0) newEntryBlock fun

        calcDomInFun :: FunCFG -> CompilerM ()
        calcDomInFun fun = do
            eliminateNotDoms (ident fun) (blocks fun)
            cs <- get -- could prbl delete next 3 lines because modify is performed in updateDomsForBlocks
            finalFun <- justLookup (ident fun) (funs cs) "calcDomInFun"
            modify(\s -> s{funs = M.insert (ident finalFun) finalFun (funs s)})
            where
                eliminateNotDoms :: String -> M.Map Label BasicBlock -> CompilerM ()
                eliminateNotDoms funIdent nodes = do
                    updateDomsForBlocks funIdent (M.elems nodes)
                    cs <- get
                    newFunCFG <- justLookup funIdent (funs cs) "eliminateNotDoms"
                    let nodes' = blocks newFunCFG
                    when (nodes /= nodes') (eliminateNotDoms funIdent nodes')

                updateDomsForBlocks :: String -> [BasicBlock]  -> CompilerM ()
                updateDomsForBlocks funIdent [] = return ()
                updateDomsForBlocks funIdent (n:ns) = do
                    when (label n /= Label 0) (do
                        updatedBlock <- updateDomsForBlock funIdent n
                        fun <- getFunCFG funIdent
                        let newBlocks = M.insert (label updatedBlock) updatedBlock (blocks fun)
                        let newFun = fun{blocks = newBlocks}
                        modify(\s -> s{funs = M.insert funIdent newFun (funs s)}))
                    updateDomsForBlocks funIdent ns

                updateDomsForBlock :: String -> BasicBlock -> CompilerM BasicBlock
                updateDomsForBlock funIdent b = do
                let preds_ = preds b
                doms_ <- S.fromList <$> mapM (getDomPreds funIdent) (S.toList preds_)
                let doms2 = myIntersect doms_
                cs <- get
                --when (label b == Label 1) (throwError $Debug ("updateDomsForBlock " ++ showDebug cs ++ "\n" ++ show (label b) ++ "\ndoms of preds" ++ show doms_ ++ "\nintersection of doms of preds" ++ show doms2))
                return $ b{domPreds = S.union (S.singleton (label b)) doms2}
                where
                    myIntersect :: Ord a => S.Set (S.Set a) -> S.Set a
                    myIntersect sets = if sets == S.empty then S.empty else foldr1 S.intersection sets

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

removeTrivPhi :: FunCFG -> CompilerM ()
removeTrivPhi fun = do
    modify (\s -> s{currFun = ident fun})--, funs = M.insert (ident fun) fun{currDefs = M.empty} (funs s)})
    blocks <- bfsOrder fun     -- bfs of CFG order
    removeTrivPhi' (ident fun) blocks
    where
    removeTrivPhi' :: String -> [Label] -> CompilerM ()
    removeTrivPhi' funIdent labels = do
        trivPhi <- findTrivPhisInBlocks labels
        case trivPhi of
            Nothing -> return ()
            Just phi -> do
                swapPhiValuesInBlocks labels phi
                removeTrivPhi' funIdent labels
    findTrivPhisInBlocks :: [Label] -> CompilerM (Maybe (Register, Value))
    findTrivPhisInBlocks [] = return Nothing
    findTrivPhisInBlocks (l:ls) = do
        (phi, newBlock) <- findRemTrivPhiInBlock l
        modifyBlockInCurrFun newBlock
        case phi of
            Nothing -> findTrivPhisInBlocks ls
            Just trivPhi -> return phi

    findRemTrivPhiInBlock :: Label -> CompilerM (Maybe (Register, Value), BasicBlock)
    findRemTrivPhiInBlock l = do
        block <- getBlockFromCurrFun l
        (mphi, newInss) <- findRemTrivPhiInInss (inss block) []
        let newBlock = block{inss = newInss}
        return (mphi, newBlock)

    findRemTrivPhiInInss :: [Instruction] -> [Instruction] -> CompilerM (Maybe (Register, Value), [Instruction])
    findRemTrivPhiInInss [] inssOut = return (Nothing, inssOut)
    findRemTrivPhiInInss (ins:inssIn) inssOut = do
        case ins of
            IPhi r t entries -> do
                case trivialEntries entries of                        
                    Just v -> do
                        return (Just (r, v), inssOut ++ inssIn)
                    Nothing -> findRemTrivPhiInInss inssIn (inssOut ++ [ins])
            _ -> findRemTrivPhiInInss inssIn (inssOut ++ [ins])
    swapPhiValuesInBlocks :: [Label] -> (Register, Value) -> CompilerM ()
    swapPhiValuesInBlocks [] _ = return ()
    swapPhiValuesInBlocks (l:ls) phi = do
        newBlock <- swapPhiValuesInBlock l phi
        modifyBlockInCurrFun newBlock
        swapPhiValuesInBlocks ls phi
    swapPhiValuesInBlock :: Label -> (Register, Value) -> CompilerM BasicBlock
    swapPhiValuesInBlock l phi = do
        block <- getBlockFromCurrFun l
        newInss <- swapPhiValuesInInss (inss block) [] phi
        return block{inss = newInss}
    swapPhiValuesInInss :: [Instruction] -> [Instruction] -> (Register, Value) -> CompilerM [Instruction]
    swapPhiValuesInInss [] inssOut _ = return inssOut
    swapPhiValuesInInss (ins:inssIn) inssOut phi = do
        newIns <- swapPhiValuesInIns ins phi
        swapPhiValuesInInss inssIn (inssOut ++ [newIns]) phi

    swapPhiValuesInIns :: Instruction -> (Register, Value) -> CompilerM Instruction
    swapPhiValuesInIns ins phi = do
        case ins of
            Call r t ident args -> do
                let args' = map (swapForPhi phi) args
                return (Call r t ident args')
            Ret v -> return $ Ret (swapForPhi phi v)
            Ari r op v1 v2 -> do
                let v1' = swapForPhi phi v1
                let v2' = swapForPhi phi v2
                return $ Ari r op v1' v2'
            Xor r v1 v2 -> do
                let v1' = swapForPhi phi v1
                let v2' = swapForPhi phi v2
                return $ Xor r v1' v2'
            Cmp r op t v1 v2 -> do
                let v1' = swapForPhi phi v1
                let v2' = swapForPhi phi v2
                return $ Cmp r op t v1' v2'
            IPhi r t entries -> do
                let entries' = swapEntriesForPhi phi entries
                return $ IPhi r t entries'
            BrCond v l1 l2 -> return $ BrCond (swapForPhi phi v) l1 l2
            _ -> return ins

    swapEntriesForPhi :: (Register, Value) -> [(Value, Label)] -> [(Value, Label)]
    swapEntriesForPhi phi entries = map (\(v, l) -> (swapForPhi phi v, l)) entries
    swapForPhi :: (Register, Value) -> Value -> Value
    swapForPhi (r2, v2) v1 = case v1 of
        (VRegister r1 t) -> if r1 == r2 then v2 else v1
        _ -> v1

        




        
gcse :: FunCFG -> CompilerM ()
gcse fun = do
    modify (\s -> s{currFun = ident fun, funs = M.insert (ident fun) fun{currDefs = M.empty} (funs s)})
    blocks' <- bfsOrder fun     -- bfs of CFG order
    gcse' fun blocks' 0
    where
        gcse' :: FunCFG -> [Label] -> Int -> CompilerM ()
        gcse' fun blocks runs = do
            mapM_ gcseInBlock blocks
            newFun <- getCurrFunCFG
            mapM_ clearSubexpressionsInBlock blocks
            when (newFun /= fun ) (gcse' newFun blocks (runs+1)) -- fixpoint
            where
                clearSubexpressionsInBlock :: Label -> CompilerM ()
                clearSubexpressionsInBlock l = do
                    block <- getBlockFromCurrFun l
                    let newBlock = block{subexpressions = M.empty}
                    modifyBlockInCurrFun newBlock
                gcseInBlock :: Label -> CompilerM ()
                gcseInBlock l = do
                    block <- getBlockFromCurrFun l
                    subexprs <- subexprsFromDoms l
                    let subexprs' = foldr M.union M.empty subexprs
                    newInss <- optimizeInss l subexprs' (inss block) []
                    newBlock <- getBlockFromCurrFun l
                    let finalBlock = newBlock{inss = newInss}
                    modifyBlockInCurrFun finalBlock

                    where
                        optimizeInss :: Label -> M.Map Instruction Register -> [Instruction] -> [Instruction] -> CompilerM [Instruction]
                        optimizeInss _ _ [] inssOut = return $ reverse inssOut
                        optimizeInss l subexprsFromDoms (ins:inssIn) inssOut = do
                            newIns <- optimizeIns l subexprsFromDoms ins
                            case newIns of
                                Just newIns' -> optimizeInss l subexprsFromDoms inssIn (newIns':inssOut)
                                Nothing -> optimizeInss l subexprsFromDoms inssIn inssOut

                        subexprsFromDoms :: Label -> CompilerM [M.Map Instruction Register]
                        subexprsFromDoms l = do
                            block <- getBlockFromCurrFun l
                            let doms = S.toList $ domPreds block
                            mapM subexprsFromBlock doms
                        subexprsFromBlock :: Label -> CompilerM (M.Map Instruction Register)
                        subexprsFromBlock l = do
                            block <- getBlockFromCurrFun l
                            return $ subexpressions block



optimizeIns :: Label -> M.Map Instruction Register -> Instruction -> CompilerM (Maybe Instruction)
optimizeIns l subexprsFromDoms ins = do
    block <- getBlockFromCurrFun l
    let gins = generifyIns ins
    let allSubexprs = M.union subexprsFromDoms (subexpressions block)
    let res = M.lookup gins allSubexprs-- find in subexpression calculated in Doms and in current Block (so far)
    --when (isJust res ) (liftIO $ putStrLn ("optimizing " ++ show ins ++ " " ++ show res)) 
    (newIns, newBlock) <- case ins of
        -- no Alloc, Store, Load, DeclareGString, Define, DefineMain, ILabel
        Ari r op v1 v2 -> do
            v1' <- readValueGCSE l v1
            v2' <- readValueGCSE l v2
            case res of
                Nothing -> do
                    writeVariable l r (VRegister r I32)
                    return (Just $ Ari r op v1' v2', block{subexpressions = M.insert gins r (subexpressions block)})
                Just calculatedExpr -> do
                    writeVariable l r (VRegister calculatedExpr I32)
                    return (Nothing, block)
        Xor r v1 v2 -> do
            v1' <- readValueGCSE l v1
            v2' <- readValueGCSE l v2
            case res of
                Nothing -> do
                    writeVariable l r (VRegister r I1)
                    return (Just $ Xor r v1' v2', block{subexpressions = M.insert gins r (subexpressions block)})
                Just calculatedExpr -> do
                    writeVariable l r (VRegister calculatedExpr I1)
                    return (Nothing, block)
        Cmp r op t v1 v2 -> do
            v1' <- readValueGCSE l v1
            v2' <- readValueGCSE l v2
            case res of
                Nothing -> do
                    writeVariable l r (VRegister r I1)
                    return (Just $ Cmp r op t v1' v2', block{subexpressions = M.insert gins r (subexpressions block)})
                Just calculatedExpr -> do
                    writeVariable l r (VRegister calculatedExpr I1)
                    return (Nothing, block)
        Ret v -> do
            v' <- readValueGCSE l v
            case res of
                Nothing -> return (Just $ Ret v', block)
                Just calculatedExpr -> return (Just $ Ret (VRegister calculatedExpr (getValueType v)), block)
        BrCond v l1 l2 -> do
            v' <- readValueGCSE l v
            case res of
                Nothing -> return (Just $ BrCond v' l1 l2, block)
                Just calculatedExpr -> return (Just $ BrCond (VRegister calculatedExpr (getValueType v)) l1 l2, block)
        Call r t ident values -> do
            when (isJust r) (writeVariable l (fromJust r) (VRegister (fromJust r) t))
            values' <- mapM (readValueGCSE l) values
            --cfg <- getCurrFunCFG
            --writeVariable l r (VRegister r t)

            --liftIO $ putStrLn (show l ++ "in call " ++ show ins ++ " v' " ++ show values' ++ "\n" ++ show (currDefs cfg))
            return (Just $ Call r t ident values', block)
        IPhi r t entries -> do
            entries' <- mapM (readEntryValGCSE l) entries
            writeVariable l r (VRegister r t)
            return (Just $ IPhi r t entries', block)
        _ -> return (Just ins, block) -- RetVoid, Br, Phi

    modifyBlockInCurrFun newBlock
    return newIns
    where
        -- change %r -> (generic) %0 in instrucions of type "%r = expression"
        -- in order to compare inss in subexpression map
        -- i.e. "%r1 = expressionX" == "%r2 = expressionX"
        generifyIns :: Instruction -> Instruction
        generifyIns ins =
            let zeroR = Register 0 in
                case ins of
                    Ari r op v1 v2 -> Ari zeroR op v1 v2
                    Xor r v1 v2 -> Xor zeroR v1 v2
                    Cmp r op t v1 v2 -> Cmp zeroR op t v1 v2
                    _ -> ins

trivialEntries :: [(Value, Label)] -> Maybe Value
trivialEntries entries =
    let vs = map fst entries in
    if length (nub vs) == 1
        then Just $ head (nub vs)
        else Nothing

readValueGCSE :: Label -> Value -> CompilerM Value
readValueGCSE l (VRegister variable t) = readVariableGCSE l variable t
readValueGCSE l v = return v

readVariableGCSE :: Label -> Register -> Type -> CompilerM Value
readVariableGCSE l variable t = do
    cs <- get
    let funIdent = currFun cs
    fun <- justLookup funIdent (funs cs) "readVariable"
    case M.lookup (l, variable) (currDefs fun) of
        Just val -> return val
        Nothing -> return $ VRegister variable t

readEntryValGCSE :: Label -> (Value, Label) -> CompilerM (Value, Label)
readEntryValGCSE label (v, l) = do
    v' <- readValueGCSE l v -- sic! l not label 
    return (v', l)
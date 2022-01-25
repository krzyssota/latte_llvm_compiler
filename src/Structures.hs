{-# LANGUAGE FlexibleInstances #-}

module Structures where
import qualified AbsLatte
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import LLVMDomain
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State


type VarsEnv = M.Map AbsLatte.Ident (LLVMDomain.Type, Register)
type CompilerM = ReaderT VarsEnv (ExceptT MyError (StateT CompilerState IO))
type RetInStmt = Bool
type CompStmtRes = (VarsEnv, RetInStmt)

--data SsaIdent = SsaIdent AbsLatte.Ident Int
--    deriving (Show, Eq)


type FnEnv = M.Map String LLVMDomain.Type -- ret type
--type FunsCode = M.Map String (M.Map Label [Instruction]) -- FnIdent -> BasicBlocks
--type CFG = M.Map Label BasicBlock

data CompilerState = CompilerState {
    registerCounter :: Counter,
    labelCounter :: Counter,
    globalsCounter :: Counter,
    globInstructions :: [Instruction],
    funs :: M.Map String FunCFG,
    fnEnv :: FnEnv,
    currFun :: String
} deriving (Show)


data FunCFG = FunCFG {
    ident :: String,
    def :: Instruction,
    blocks :: M.Map Label BasicBlock,
    currLabel :: Label,
    currDefs :: M.Map (Register, Label) Value
} deriving Show

data BasicBlock = BasicBlock {
    label :: Label,
    printableLabel :: Bool,
    phis :: M.Map Register (Type, [(Value, Label)]),
    inss :: [Instruction],
    varsDeclaredPrev :: S.Set AbsLatte.Ident,
    varsDeclared :: S.Set AbsLatte.Ident,
    varIdents :: M.Map AbsLatte.Ident Int,
    alive :: [AbsLatte.Ident],
    preds :: S.Set Label,
    succs :: S.Set Label,
    domPreds :: S.Set Label,
    domSuccs :: S.Set Label,
    imDomSuccs :: S.Set Label,
    imDomPred :: Maybe Label
} deriving (Show, Eq)

newBlock :: Label -> BasicBlock
newBlock (Label 0) = newBlock' (Label 0) False
newBlock l = newBlock' l True
newBlock' :: Label -> Bool -> BasicBlock
newBlock' l b = BasicBlock {
    label = l, printableLabel = b, phis = M.empty,
    inss = [],
    varsDeclaredPrev = S.empty, varsDeclared = S.empty, varIdents = M.empty,
    alive = [], preds = S.empty, succs = S.empty, domPreds = S.empty, domSuccs = S.empty,
    imDomPred = Nothing, imDomSuccs = S.empty}

getInsFromPhis :: BasicBlock -> [Instruction]
getInsFromPhis b = map getInsFromPhi (M.toList $ phis b)

getInsFromPhi :: (Register, (Type, [(Value, Label)])) -> Instruction
getInsFromPhi (r, (t, entries)) = Phi r t entries 


modifyBlockInCurrFun :: BasicBlock -> CompilerM ()
modifyBlockInCurrFun b = do
    fun <- getCurrFunCFG
    let newFun = addUpdateBlockInCFG (label b) b fun
    modify (\s -> s{funs = M.insert (ident newFun) newFun (funs s)})

getDomPreds :: String -> Label -> CompilerM (S.Set Label)
getDomPreds funIdent l = do
    b <- getBlock funIdent l
    return $ domPreds b

addImDomPred :: String -> Label -> Label -> CompilerM()
addImDomPred funIdent predLabel label = do
    block <- getBlock funIdent label
    let newBlock = block{imDomPred = Just predLabel}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG label newBlock funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)})

addImDomSucc :: String -> Label -> Label -> CompilerM FunCFG
addImDomSucc funIdent label succIdent = do
    block <- getBlock funIdent label
    let newBlock = block{imDomSuccs = S.insert label (imDomSuccs block)}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG label newBlock funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)}) -- pewnie nie trzeba tutaj
    return newFunCFG

getPreds :: String -> Label -> CompilerM (S.Set Label)
getPreds funIdent l = do
    b <- getBlock funIdent l
    return $ preds b

addSuccInDomTree :: String -> Label -> Label -> CompilerM()
addSuccInDomTree funIdent labelSucc label = do
    b <- getBlock funIdent label
    let newBlock = b{domSuccs = S.insert labelSucc (domSuccs b)}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG label newBlock funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)})

addEdge :: Label -> Label -> String -> CompilerM ()
addEdge labelFrom labelTo funIdent = do
    addSuccToBlock labelFrom labelTo funIdent
    addPredToBlock labelFrom labelTo funIdent

addPredToBlock :: Label -> Label -> String -> CompilerM ()
addPredToBlock labelFrom labelTo funIdent = do
    blockTo <- getBlock funIdent labelTo
    let newBlockTo = blockTo{preds = S.insert labelFrom (preds blockTo)}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG labelTo newBlockTo funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)})

addSuccToBlock ::  Label -> Label -> String -> CompilerM()
addSuccToBlock labelFrom labelTo funIdent= do
    blockFrom <- getBlock funIdent labelFrom
    let newBlockFrom = blockFrom{succs = S.insert labelTo (succs blockFrom)}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG labelFrom newBlockFrom funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)})

addVars :: Label -> Label -> String -> CompilerM ()
addVars labelFrom labelTo funIdent = do
    blockFrom <- getBlock funIdent labelFrom
    let vars = S.union (varsDeclared blockFrom) (varsDeclaredPrev blockFrom)
    addVarsToBlock vars labelTo funIdent

addVarsToBlock :: S.Set AbsLatte.Ident -> Label -> String -> CompilerM ()
addVarsToBlock vs l funIdent = do
    b <- getBlock funIdent l
    let newBlock = b{varsDeclaredPrev = S.union (varsDeclaredPrev b) vs}
    funCFG <- getFunCFG funIdent
    let newFunCFG = addUpdateBlockInCFG l newBlock funCFG
    modify (\s -> s{funs = M.insert funIdent newFunCFG (funs s)})

-- insert or update
addUpdateBlockInCFG :: Label -> BasicBlock -> FunCFG -> FunCFG
addUpdateBlockInCFG label block cfg = cfg {blocks = M.insert label block (blocks cfg)}

changeCurrLabelInCFG :: Label -> FunCFG -> FunCFG
changeCurrLabelInCFG l cfg = cfg {currLabel = l}

addInsToBlock :: Instruction  -> BasicBlock -> BasicBlock
addInsToBlock i b = b {inss = inss b ++ [i]}

getCurrBlock :: CompilerM BasicBlock
getCurrBlock = do
    cs <- get
    currFunCFG <- getCurrFunCFG
    let label = currLabel currFunCFG
    getBlock (currFun cs) label

getBlock :: String -> Label -> CompilerM BasicBlock
getBlock ident l = do
    funCFG <- getFunCFG ident
    let blocks_ = blocks funCFG
    justLookup l blocks_ ("[getBlock] label in blocks " ++ show l ++ "\n"++ show blocks_)

getBlockFromCurrFun :: Label -> CompilerM BasicBlock
getBlockFromCurrFun l = do
    cs <- get
    fun <- getFunCFG (currFun cs)
    let bs = blocks fun
    justLookup l bs ("[getBlockFromCurrFun] label in blocks " ++ show l ++ "\n"++ show bs)

getCurrFunCFG :: CompilerM FunCFG
getCurrFunCFG = do
    cs <- get
    getFunCFG $ currFun cs

getFunCFG :: String -> CompilerM FunCFG
getFunCFG ident = do
    cs <- get
    let funs_ = funs cs
    justLookup ident funs_ ("[getCFunCFG] currFunIdent in funs" ++ show ident ++ show funs_)

addToVarsDeclared :: AbsLatte.Ident -> CompilerM ()
addToVarsDeclared ident = do
    currBlock <- getCurrBlock
    let newBlock = currBlock {varsDeclared = S.insert ident (varsDeclared currBlock)}
    currFunCFG <- getCurrFunCFG
    let label = currLabel currFunCFG
    let newFunCFG = addUpdateBlockInCFG label newBlock currFunCFG
    modify (\s -> s{funs = M.insert (currFun s) newFunCFG (funs s)})


showCompiledCode :: CompilerState -> String
showCompiledCode s = showFuns $ M.elems (funs s)

showFuns :: [FunCFG] -> String
showFuns f = concatMap showFun f

showFun :: FunCFG -> String
showFun f = show (def f) ++ "\n" ++ showBlocks (M.elems (blocks f)) ++ "}\n"

showBlocks :: [BasicBlock] -> String
showBlocks bs = concatMap showBlock bs

showBlock :: BasicBlock -> String
showBlock b = (if printableLabel b then show (ILabel $ label b) else "") ++ showInss (inss b)

showInss :: [Instruction] -> String
showInss inss = concatMap showIns inss

showIns :: Instruction -> String
showIns ins = show ins ++ "\n"

getCurrLabel :: CompilerM Label 
getCurrLabel = do
    cs <- get
    let currFunIdent = currFun cs
    currFunCFG <- justLookup currFunIdent (funs cs) ""
    return (currLabel currFunCFG)

justLookup :: (Ord a) => a -> M.Map a b -> String -> CompilerM b
justLookup k m debug = case M.lookup k m of
    Nothing -> throwError $ BackBug ("lookup fail " ++ debug)
    Just x -> return x

newtype Counter = Counter Int
instance Show Counter where
    show (Counter i) = "Counter " ++ show i
newCounter :: Counter
newCounter = Counter 1
incCounter :: Counter -> Counter
incCounter (Counter i) = Counter $ i+1
castCounter :: Counter -> Int
castCounter (Counter i) = i


type MyError = MyError' AbsLatte.BNFC'Position
data MyError' a = NotImplemented String | FrontBug String | Debug String | BackBug String
instance Show MyError where
    show (NotImplemented s) = "NotImplemented " ++ s
    show (FrontBug s) = "FrontBug " ++ s
    show (Debug s) = "Debug " ++ s
    show (BackBug s) = "BackBug " ++ s

defaultRes :: CompilerM CompStmtRes
defaultRes = do
    env <- ask
    return (env, False)

getNext :: CompilerM Int
getNext = do
    cs <- get
    let old = castCounter (registerCounter cs) in do
        modify (\s -> s {registerCounter = incCounter $ registerCounter cs})
        return old
newRegister :: CompilerM Register
newRegister = do
    --Register <$> getNext
    cs <- get
    let old = castCounter (registerCounter cs) in do
        modify (\s -> s {registerCounter = incCounter $ registerCounter cs})
        return $ Register old
newLabel :: CompilerM Label
newLabel = do
    --Label <$> getNext
    cs <- get
    let old = castCounter (labelCounter cs) in do
        modify (\s -> s {labelCounter = incCounter $ labelCounter cs})
        return $ Label old
newGlobal :: CompilerM Int
newGlobal = do
    cs <- get
    let old = castCounter (globalsCounter cs) in do
        modify (\s -> s {globalsCounter = incCounter $ globalsCounter cs})
        return old

showDebug :: CompilerState -> String
showDebug s = showDebugFuns $ M.elems (funs s)

showDebugFuns :: [FunCFG] -> String
showDebugFuns f = concatMap showDebugFun f

showDebugFun :: FunCFG -> String
showDebugFun f = show (def f) ++ "\n" ++ showDebugBlocks (M.elems (blocks f)) ++ "}\n"

showDebugBlocks :: [BasicBlock] -> String
showDebugBlocks bs = concatMap showDebugBlock bs

showDebugBlock :: BasicBlock -> String
showDebugBlock b = show (label b)
    ++ "\nphis " ++ show (phis b)
    ++ "\nsuccs" ++ show (succs b)
    ++ "\npreds" ++ show (preds b)
    {- ++ "\nvarsDeclPrev" ++ show (varsDeclaredPrev b)
    ++ "\nvarsDecl" ++ show (varsDeclared b)
  
    ++ "\ndomPreds" ++ show (domPreds b)
    ++ "\ndomSuccs" ++ show (domSuccs b)
    ++ "\ndomImPred" ++ show (imDomPred b)
    ++ "\ndomImSuccs" ++ show (imDomSuccs b) -}
    ++ "\n\n"
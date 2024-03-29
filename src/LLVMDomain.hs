
module LLVMDomain where
import Data.List (intercalate)
import qualified AbsLatte

type Loc = Int

data Type = I32 | Void | Undef | I8 | I1 | Ptr Type
    deriving (Eq, Ord)
instance Show Type where
    show I32 = "i32"
    show Void = "void"
    show Undef = "undef"
    show I8 = "i8"
    show (Ptr t) = show t ++ "*"
    show I1 = "i1"

llvmType :: AbsLatte.Type -> Type
llvmType (AbsLatte.Int _) = I32
llvmType (AbsLatte.Str _) = Ptr I8
llvmType (AbsLatte.Bool _) = I1
llvmType (AbsLatte.Void _) = Void
llvmType _ = Undef

llvmArg :: AbsLatte.Arg -> (Type, AbsLatte.Ident)
llvmArg (AbsLatte.Arg _ t ident) = (llvmType t, ident)

llvmAddOp :: AbsLatte.AddOp -> AriOp
llvmAddOp addOp = case addOp of
                    AbsLatte.Plus _ -> Add
                    AbsLatte.Minus _ -> Sub
llvmMulOp :: AbsLatte.MulOp -> AriOp
llvmMulOp mulOp = case mulOp of
                    AbsLatte.Times _ -> Mul
                    AbsLatte.Div _ -> Sdiv
                    AbsLatte.Mod _ -> Srem

newtype Register = Register Int
    deriving (Eq, Ord)
instance Show Register where
    show (Register i) = "%r_" ++ show i

data AriOp = Add | Sub | Mul | Sdiv | Srem
    deriving (Eq, Ord)
instance Show AriOp where
    show Add = "add nsw i32"
    show Sub = "sub nsw i32"
    show Mul = "mul nsw i32"
    show Sdiv = "sdiv i32"
    show Srem = "srem i32"

data RelOp = EQ | NE | SGT | SGE | SLT | SLE
    deriving (Eq, Ord)
instance Show RelOp where
    show LLVMDomain.EQ = "eq"
    show NE = "ne"
    show SGT = "sgt"
    show SGE = "sge"
    show SLT = "slt"
    show SLE = "sle"

llvmRelOp :: AbsLatte.RelOp -> RelOp
llvmRelOp (AbsLatte.LTH _) = SLT
llvmRelOp (AbsLatte.LE _) = SLE
llvmRelOp (AbsLatte.GTH _) = SGT
llvmRelOp (AbsLatte.GE _) = SGE
llvmRelOp (AbsLatte.EQU _) = LLVMDomain.EQ
llvmRelOp (AbsLatte.NE _) = NE

data Const = ConstI Int | ConstT | ConstF
    deriving (Eq, Ord)
instance Show Const where
    show (ConstI i) = show i
    show ConstT = "true"
    show ConstF = "false"

data Value = VConst Const
    | VRegister Register Type
    | GetElemPtr Loc Int
    deriving (Eq, Ord)
getValueType :: Value -> Type 
getValueType (VConst (ConstI _)) =  I32
getValueType (VConst ConstT) =  I1
getValueType (VConst ConstF) =  I1
getValueType (VRegister _ t) =  t
getValueType (GetElemPtr loc len) =  Ptr I8
instance Show Value where
    show (VConst c) = show c
    show (VRegister r _) = show r
    show (GetElemPtr loc len) = "getelementptr inbounds ([" ++ show len ++ "x i8], ["
        ++ show len ++ " x i8]* " ++ "@" ++ stringPrefix ++ show loc ++ ", i32 0, i32 0)"
       
showValueType :: Value -> String 
showValueType = show . getValueType

newtype Label = Label Int
    deriving (Eq, Ord)
instance Show Label where
    show (Label i) = "l_" ++ show i

data Instruction =
    Call (Maybe Register) Type String [Value] 
    | Ret Value
    | RetVoid
    | Alloc Register Type
    | Store Value Type Register
    | Load Register Type Register
    | DeclareGString Loc String Int
    | Ari Register AriOp Value Value
    | Xor Register Value Value
    | Cmp Register RelOp Type Value Value
    | BrCond Value Label Label
    | Br Label
    | ILabel Label
    | IPhi Register Type [(Value, Label)]
    | Define Type String [(Type, Register)]
    | DefineMain
               deriving (Eq, Ord)

instance Show Instruction where
    show (Call Nothing retType ident args) =
        "\t" ++ "call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Call (Just r) retType ident args ) =
        show r ++ " = call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Ret v) =
        "\t" ++ "ret " ++ showValueType v ++ " " ++ show v
    show RetVoid = "\t" ++"ret void"
    show (Alloc r t) = "\t" ++show r ++ " = alloca " ++ show t
    show (Store v t r) = "\t" ++"store " ++ show t ++ " " ++ show v ++ ", " ++ show (Ptr t) ++ " " ++ show r
    show (Load r t r2) = "\t" ++show r ++ " = load " ++ show t ++ ", " ++ show (Ptr t) ++ " " ++ show r2
    show (DeclareGString loc s len) = "\t" ++"@" ++ stringPrefix ++ show loc ++ " = private unnamed_addr constant ["
        ++ show len ++ "x i8] c\"" ++ escape s ++ "\\00\", align 1"
    show (Ari r op v1 v2) = "\t" ++show r ++ " = " ++ show op ++ " " ++ show v1 ++ ", " ++ show v2
    show (Xor r v1 v2) = "\t" ++show r ++ " = xor i1 " ++ show v1 ++ ", " ++ show v2
    show (Cmp r op t v1 v2) = "\t" ++show r ++ " = icmp " ++ show op ++ " " ++ show t ++ " " ++ show v1 ++ ", " ++ show v2
    show (BrCond v l1 l2) = "\t" ++"br i1 " ++ show v ++ ", label %" ++ show l1 ++ ", label %" ++ show l2
    show (Br l) = "\t" ++"br label %" ++ show l
    show (ILabel l) = show l ++ ":"
    show (IPhi r t pairs) = "\t" ++show r ++ " = phi " ++ show t ++ intercalate ", " (map showPair pairs)
        where showPair (v, l) = "[ " ++ show v ++ ", %" ++ show l ++ " ]"
    show (Define t ident args) = "define " ++ show t ++ " @" ++ ident ++ "(" ++ intercalate ", " (map showPair args) ++ ") {"
        where showPair (t, r) = show t ++ " " ++ show r
    show DefineMain = "define i32 @main(i32 %argc, i8** %argv) {"


stringPrefix = ".str."

showFnArg :: Value -> String
showFnArg v = showValueType v ++ " " ++ show v

escape :: String -> String
escape [] = []
escape ('\\' : s) = "\\" ++ "5C"  ++ escape s
escape ('\n' : s) = "\\" ++ "0A" ++ escape s
escape ('\t' : s) = "\\" ++ "09" ++ escape s
escape ('"' : s) = "\\" ++ "22" ++ escape s
escape (c : s) = c : escape s
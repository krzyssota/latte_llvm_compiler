
module LLVMDomain where
import Data.List (intercalate)
import qualified AbsLatte

type Loc = Integer

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

data Register = Register Integer
    deriving (Eq, Ord)
instance Show Register where
    show (Register i) = "%" ++ show i

data Const = ConstI Integer | ConstT | ConstF
    deriving (Eq, Ord)
instance Show Const where
    show (ConstI i) = show i
    show ConstT = "1"
    show ConstF = "0"

data Value = VConst Const
    | VRegister Register
    | GetElemPtr Loc Int
    deriving (Eq, Ord)
instance Show Value where
    show (VConst c) = show c
    show (VRegister r) = show r
    show (GetElemPtr loc len) = "getelementptr inbounds ([" ++ show len ++ "x i8], ["
        ++ show len ++ " x i8]* " ++ "@" ++ stringPrefix ++ show loc ++ ", i32 0, i32 0)"

data Instruction =
    Call Type String [(Type, Value)] (Maybe Register)
    | Ret Type Value
    | VoidRet
    | Alloc Register Type
    | Store Value Type Loc
    | Load Register Type Loc
    | DeclareGString Loc String Int
               deriving Eq

instance Show Instruction where
    show (Call retType ident args Nothing) = 
        "call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Call retType ident args (Just r)) = 
        show r ++ " = call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Ret t v) =
        "ret " ++ show t ++ " " ++ show v
    show VoidRet = show "ret void"
    show (Alloc r t) = show r ++ " = alloca " ++ show t
    -- store value from register/constant in location
    show (Store v t loc) = "store " ++ show t ++ " " ++ show v ++ ", " ++ show (Ptr t) ++ " %" ++ show loc 
    show (Load r t loc) = show r ++ " = load " ++ show t ++ ", " ++ show (Ptr t) ++ " %" ++ show loc
    show (DeclareGString loc s len) = "@" ++ stringPrefix ++ show loc ++ " = private unnamed_addr constant ["
        ++ show len ++ "x i8] c\"" ++ escape s ++ "\\00\", align 1"

stringPrefix = ".str."

showFnArg :: (Type, Value) -> String
showFnArg (t, v) = show t ++ " " ++ show v

escape :: String -> String 
escape [] = []
escape ('\\' : s) = "\\" ++ "5C"  ++ escape s
escape ('\n' : s) = "\\" ++ "0A" ++ escape s
escape ('\t' : s) = "\\" ++ "09" ++ escape s
escape ('"' : s) = "\\" ++ "22" ++ escape s
escape (c : s) = c : escape s


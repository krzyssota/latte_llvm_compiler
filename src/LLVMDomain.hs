
module LLVMDomain where
import Data.List (intercalate)
import qualified AbsLatte


data Type = I32 | Void | Undef | I8 | Ptr Type
    deriving (Eq, Ord)
instance Show Type where
    show I32 = "i32"
    show Void = "void"
    show Undef = "undef"
    show I8 = "i8"
    show (Ptr t) = show t ++ "*"

data Register = Register Integer
    deriving (Eq, Ord)
instance Show Register where
    show (Register i) = "%" ++ show i

data Instruction =
    Call Type String [(Type, Value)] (Maybe Register)
    | Ret Type Value
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

data Value = Const Integer | VRegister Register
    deriving (Eq, Ord)
instance Show Value where
    show (Const i) = show i
    show (VRegister r) = show r

showFnArg :: (Type, Value) -> String
showFnArg (t, v) = show t ++ " " ++ show v


llvmType :: AbsLatte.Type -> Type
llvmType (AbsLatte.Int a) = I32
llvmType _ = Undef

module LLVM where
import Data.List (intercalate)


data Type = I32 | Void
    deriving (Eq, Ord)
instance Show Type where
    show I32 = "i32"
    show Void = "void"

data Instruction =
    Call Type String [(Type, Value)]
    | Ret Type Value
               deriving Eq

instance Show Instruction where
    show (Call retType ident args) = 
        "call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Ret t v) =
        "ret " ++ show t ++ " " ++ show v

data Value = Const Integer
    deriving (Eq, Ord)
instance Show Value where
    show (Const i) = show i

showFnArg :: (Type, Value) -> String
showFnArg (t, v) = show t ++ " " ++ show v
    
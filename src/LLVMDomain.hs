
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

data Register = Register Integer
    deriving (Eq, Ord)
instance Show Register where
    show (Register i) = "%r_" ++ show i

data AriOp = Add | Sub | Mul | Sdiv | Srem
    deriving (Eq)
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

data Const = ConstI Integer | ConstT | ConstF
    deriving (Eq, Ord)
instance Show Const where
    show (ConstI i) = show i
    show ConstT = "true"
    show ConstF = "false"

data Value = VConst Const
    | VRegister Register
    | GetElemPtr Loc Int
    deriving (Eq, Ord)
instance Show Value where
    show (VConst c) = show c
    show (VRegister r) = show r
    show (GetElemPtr loc len) = "getelementptr inbounds ([" ++ show len ++ "x i8], ["
        ++ show len ++ " x i8]* " ++ "@" ++ stringPrefix ++ show loc ++ ", i32 0, i32 0)"

newtype Label = Label Integer
    deriving (Eq, Ord)
instance Show Label where
    show (Label i) = "l_" ++ show i

data Instruction =
    Call Type String [(Type, Value)] (Maybe Register)
    | Ret Type Value
    | RetVoid
    | Alloc Register Type
    {- | Store Value Type Loc
    | Load Register Type Loc
    | DeclareGString Loc String Int -}
    | Store Value Type Register
    | Load Register Type Register
    | DeclareGString Loc String Int
    | Ari Register AriOp Value Value
    | Xor Register Value Value
    | Cmp Register RelOp Type Value Value
    | BrCond Value Label Label
    | Br Label
    | ILabel Label
    | Phi Register Type [(Value, Label)]
    | Define Type String [(Type, Register)]
    | ClosingBracket
    | DefineMain
    | AllocStoreArg Type Register Register

               deriving Eq

instance Show Instruction where
    show (Call retType ident args Nothing) =
        "\t" ++ "call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Call retType ident args (Just r)) =
        show r ++ " = call " ++ show retType ++ " @" ++ ident
        ++ " (" ++ intercalate ", " (map showFnArg args) ++ ")"
    show (Ret t v) =
        "\t" ++ "ret " ++ show t ++ " " ++ show v
    show RetVoid = "\t" ++"ret void"
    show (Alloc r t) = "\t" ++show r ++ " = alloca " ++ show t
    -- store value from register/constant in location
   {-  show (Store v t loc) = "store " ++ show t ++ " " ++ show v ++ ", " ++ show (Ptr t) ++ " %" ++ show loc 
    show (Load r t loc) = show r ++ " = load " ++ show t ++ ", " ++ show (Ptr t) ++ " %" ++ show loc -}
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
    show (Phi r t pairs) = "\t" ++show r ++ " = phi " ++ show t ++ intercalate ", " (map showPair pairs)
        where showPair (v, l) = "[ " ++ show v ++ ", %" ++ show l ++ " ]"
    show (Define t ident args) = "define " ++ show t ++ " @" ++ ident ++ "(" ++ intercalate ", " (map showPair args) ++ ") {"
        where showPair (t, r) = show t ++ " " ++ show r
    show ClosingBracket = "}"
    show DefineMain = "define i32 @main(i32 %argc, i8** %argv) {"
    show (AllocStoreArg t r1 r2) = "\t" ++show (Alloc r2 t) ++ "\n" ++ show (Store (VRegister r1) t r2)


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

{-
goto L2
L1: body
L2: condition code, result in t
if t goto L1


	if (b) {
		int x = 1;
	}
	return 2;


  br i1 %4, label %5, label %6

5:                                                ; preds = %0
  store i32 1, i32* %2, align 4
  br label %6

6:                                                ; preds = %5, %0
  ret i32 2




if (cond){
	return 1;
} else {
	return 0;
}


define i32 @f() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8, align 1
  %3 = load i8, i8* %2, align 1

  %4 = trunc i8 %3 to i1
  br i1 %4, label %5, label %6    ; if cond jmp 5 else jmp 6

5:  then block                                      ; preds = %0
  store i32 1, i32* %1, align 4
  br label %7

6:   else block                                   ; preds = %0
  store i32 0, i32* %1, align 4
  br label %7

7:   after block                                  ; preds = %6, %5
  %8 = load i32, i32* %1, align 4
  ret i32 %8
}


int main() {
    bool b1 = true, b2 = false;
    if (b1 || b2) {
        return ;
    } else {
        return ;
    }
}

define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8, align 1
  %3 = alloca i8, align 1
  store i32 0, i32* %1, align 4
  store i8 1, i8* %2, align 1     ; b1 = true
  store i8 0, i8* %3, align 1     ; b2 = false;

  %4 = load i8, i8* %2, align 1
  %5 = trunc i8 %4 to i1
  br i1 %4, label %8, label %5    ; if b1 jmp 8 already_good else jmp 5 check_more

5: check_more                                               ; preds = %0
  %6 = load i8, i8* %2, align 1
  %7 = trunc i8 %6 to i1
  br i1 %7, label %8, label %9   ; if b2 jmp 8 good else jmp 9 else

8: if already good                                                ; preds = %5, %0
  br label %10                  ; jmp 10 end

9: else                                                ; preds = %5
  br label %10                  ; step 10 end

10:                                               ; preds = %9, %8
  ret void

-}
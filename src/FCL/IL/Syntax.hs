module FCL.IL.Syntax where

data ILName = ILName String Int
  deriving (Eq, Ord)

instance Show ILName where
  show (ILName v i) = v ++ "_" ++ show i

data ILLevel = Thread | Block | Grid
  deriving Show
data ILType = ILInt | ILDouble | ILBool | ILString | ILArray ILType
  deriving (Ord, Eq, Show)

-- type Op = String

data ILExp =
    EInt Int
  | EDouble Double
  | EBool Bool
  | EString String
  | EVar ILName
  | EIndex ILName ILExp
--  | EOp Op [ILExp]
  | EUnaryOp UnaryOp ILExp
  | EBinOp BinOp ILExp ILExp
  | EIf ILExp ILExp ILExp
 deriving (Eq, Show, Ord)

isLiteral :: ILExp -> Bool
isLiteral (EInt _)    = True
isLiteral (EDouble _) = True
isLiteral (EBool _)   = True
isLiteral (EString _)   = True 
isLiteral _           = False

isVar :: ILExp -> Bool
isVar (EVar _)  = True
isVar _         = False

data BinOp =
    AddI | SubI | MulI | DivI | ModI |
    AddD | SubD | MulD | DivD |
    LtI | LteI | GtI | GteI | EqI | NeqI | 
    LtD | LteD | GtD | GteD | EqD | NeqD |
    And | Or |
    Land | Lor | Xor | Sll | Srl | -- bitwise ops
    MinI | MaxI
  deriving (Eq, Show, Ord)

data UnaryOp = SignI | AbsI | AbsD
  deriving (Eq, Show, Ord)

data Stmt a =
    Declare ILName ILType ILExp a
  | Alloc ILName ILType ILExp a
  | Distribute ILLevel ILName ILExp [Stmt a] a
  | ParFor ILLevel ILName ILExp [Stmt a] a
  | Synchronize a
  | Assign ILName ILExp a
  | AssignSub ILName ILExp ILExp a
  | If ILExp [Stmt a] [Stmt a] a
  | While ILExp [Stmt a] a
  | SeqFor ILName ILExp [Stmt a] a
  | ReadIntCSV ILName ILName ILExp a
  | PrintIntArray ILExp ILExp a
  | Benchmark ILExp [Stmt a] a
 deriving Show

type ILProgram a = [Stmt a]

labelOf :: Stmt a -> a
labelOf stmt =
  case stmt of
    Declare _ _ _ lbl      -> lbl
    Alloc _ _ _ lbl        -> lbl
    Distribute _ _ _ _ lbl -> lbl
    ParFor _ _ _ _ lbl     -> lbl
    Synchronize lbl        -> lbl
    Assign _ _ lbl         -> lbl
    AssignSub _ _ _ lbl    -> lbl
    If _ _ _ lbl           -> lbl
    While _ _ lbl          -> lbl
    SeqFor _ _ _ lbl       -> lbl
    ReadIntCSV _ _ _ lbl   -> lbl
    PrintIntArray _ _ lbl  -> lbl
    Benchmark _ _ lbl      -> lbl

module Language.GPUIL.Syntax where

import Data.Word (Word8, Word32, Word64)

-- Types
data Attribute =
    Volatile
  | Local
  | Global
 deriving (Eq, Show, Ord)
          
data CType =
    CInt32
  | CDouble
  | CBool
  | CWord8
  | CWord32
  | CWord64
  | CPtr [Attribute] CType -- ^ Only put attributes on the outermost CPtr, if nested!
 deriving (Eq, Show, Ord)

sizeOf :: CType -> Int
sizeOf CInt32 = 4
sizeOf CDouble = 8
sizeOf CBool = 4 -- we represent bools as uint32
sizeOf CWord8 = 1
sizeOf CWord32 = 4
sizeOf CWord64 = 4
sizeOf (CPtr _ _) = error "TODO: The device has to be queried for pointer-size"

-- Variables
type VarName = (String, CType)

-- Builtin operators
data UnaryOp =
    Not | I2D | NegateInt | NegateDouble |
    NegateBitwise |
    Ceil | Floor | Exp | Ln | AbsI | AbsD
  deriving (Eq, Show, Ord)

data BinOp =
    AddI | SubI | MulI | DivI | ModI |
    AddD | SubD | MulD | DivD |
    AddPtr |
    LtI | LteI | GtI | GteI | EqI | NeqI | 
    LtD | LteD | GtD | GteD | EqD | NeqD |
    And | Or |
    Land | Lor | Xor | Sll | Srl -- bitwise ops
  deriving (Eq, Show, Ord)

-- TODO: add function call?
data IExp = 
    IntE Int
  | DoubleE Double
  | BoolE Bool
  | Word8E Word8
  | Word32E Word32
  | Word64E Word64
  | VarE VarName
  | UnaryOpE UnaryOp IExp
  | BinOpE BinOp IExp IExp
  | IfE IExp IExp IExp
  | IndexE VarName IExp
  | CastE CType IExp
  | GlobalID | LocalID | GroupID
  | LocalSize | NumGroups | WarpSize
 deriving (Eq, Show, Ord)

data Statement a =
    For VarName IExp [Statement a] a
  | SeqWhile IExp [Statement a] a
  | If IExp [Statement a] [Statement a] a
  | Assign VarName IExp a
  | AssignSub VarName IExp IExp a
  | Decl VarName IExp a
  | SyncLocalMem a
  | SyncGlobalMem a
  | Comment String a
  | Allocate VarName IExp a
 deriving (Eq, Show)

labelOf :: Statement t -> t
labelOf (For _ _ _ lbl) = lbl
labelOf (SeqWhile _ _ lbl) = lbl
labelOf (If _ _ _ lbl) = lbl
labelOf (Assign _ _ lbl) = lbl
labelOf (AssignSub _ _ _ lbl) = lbl
labelOf (Decl _ _ lbl) = lbl
labelOf (SyncLocalMem lbl) = lbl
labelOf (SyncGlobalMem lbl) = lbl
labelOf (Comment _ lbl) = lbl
labelOf (Allocate _ _ lbl) = lbl


data Kernel =
  Kernel { kernelName :: String
         , kernelParams :: [VarName]
         , kernelBody :: [Statement ()]
         , kernelSharedMem :: Maybe IExp
         , kernelBlockSize :: Maybe Int
         }
  deriving (Eq, Show)

isScalar :: IExp -> Bool
isScalar (IntE _)    = True
isScalar (DoubleE _) = True
isScalar (BoolE _)   = True 
isScalar (Word8E _)  = True
isScalar (Word32E _) = True
isScalar _           = False

isVar :: IExp -> Bool
isVar (VarE _)  = True
isVar _         = False

removeLabels :: [Statement a] -> [Statement ()]
removeLabels stmts = map rm stmts
  where
    rm :: Statement a -> Statement ()
    rm (For v e ss _)          = For v e           (map rm ss) ()
    rm (SeqWhile v ss _)       = SeqWhile v        (map rm ss) ()
    rm (If e ss0 ss1 _)        = If e              (map rm ss0) (map rm ss1) ()
    rm (Assign v e _)          = Assign v e        ()
    rm (AssignSub v e0 e1 _)   = AssignSub v e0 e1 ()
    rm (Decl v e _)            = Decl v e          ()
    rm (SyncLocalMem _)        = SyncLocalMem      ()
    rm (SyncGlobalMem _)       = SyncGlobalMem     ()
    rm (Comment msg _)         = Comment msg       ()
    rm (Allocate v e _)        = Allocate v e      ()

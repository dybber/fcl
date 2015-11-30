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

data Level = Thread | Warp | Block | Grid
 deriving (Eq, Show)

type Statements a = [(Statement a,a)]

data Statement a =
    For VarName IExp (Statements a)
  | DistrPar Level VarName IExp (Statements a)
  | ForAll Level VarName IExp (Statements a)
  | SeqWhile IExp (Statements a)
  | If IExp (Statements a) (Statements a)
  | Assign VarName IExp
  | AssignSub VarName IExp IExp
  | Decl VarName IExp
  | SyncLocalMem
  | SyncGlobalMem
  | Comment String
  | Allocate VarName IExp
 deriving (Eq, Show)

data Kernel =
  Kernel { kernelName :: String
         , kernelParams :: [VarName]
         , kernelBody :: Statements ()
         , kernelSharedMem :: Maybe IExp
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
isVar GlobalID  = True
isVar LocalID   = True
isVar GroupID   = True
isVar LocalSize = True
isVar NumGroups = True
isVar WarpSize  = True
isVar _         = False

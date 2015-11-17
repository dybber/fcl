module Language.GPUIL.Syntax where

import Data.Word (Word8, Word32)

-- Types
data Attribute =
    Volatile
  | Local
  | Global
 deriving (Eq, Show, Ord)
          
data CType =
    Int32T
  | DoubleT
  | BoolT
  | Word8T
  | Word32T
  | Ptr [Attribute] CType -- ^ Only put attributes on the outermost Ptr, if nested!
 deriving (Eq, Show, Ord)

sizeOf :: CType -> Int
sizeOf Int32T = 4
sizeOf DoubleT = 8
sizeOf BoolT = 4 -- we represent bools as uint32
sizeOf Word8T = 1
sizeOf Word32T = 4
sizeOf (Ptr _ _) = error "TODO: The device has to be queried for pointer-size"

-- For untyped terms. They will have type "IExp NoType"
data NoType = NoType
  deriving (Eq, Show)

-- Variables
type VarName = (String, CType)

-- Builtin operators
data UnaryOp =
    Not | I2D | NegateInt | NegateDouble |
    NegateBitwise |
    Ceil | Floor | Exp | Ln | AbsI | AbsD
  deriving (Eq, Show)

data BinOp =
    AddI | SubI | MulI | DivI | ModI |
    AddD | SubD | MulD | DivD | 
    LtI | LteI | GtI | GteI | EqI | NeqI | 
    LtD | LteD | GtD | GteD | EqD | NeqD |
    And | Or |
    Land | Lor | Xor | Sll | Srl -- bitwise ops
  deriving (Eq, Show)

-- TODO: add function call?
-- TODO: why casting?
data IExp ty = 
    IntE Int
  | DoubleE Double
  | BoolE Bool
  | Word8E Word8
  | Word32E Word32
  | VarE VarName ty
  | UnaryOpE UnaryOp (IExp ty)
  | BinOpE BinOp (IExp ty) (IExp ty)
  | IfE (IExp ty) (IExp ty) (IExp ty) ty
  | IndexE VarName (IExp ty)
  | CastE CType (IExp ty)
  | GlobalID | LocalID | GroupID
  | LocalSize | NumGroups | WarpSize
 deriving (Eq, Show)

data Level = Thread | Warp | Block | Grid
 deriving (Eq, Show)

type Statements a ty = [(Statement a ty,a)]

data Statement a ty =
    For VarName (IExp ty) (Statements a ty)
  | DistrPar Level VarName (IExp ty) (Statements a ty)
  | ForAll Level VarName (IExp ty) (Statements a ty)
  | SeqWhile (IExp ty) (Statements a ty)
  | If (IExp ty) (Statements a ty) (Statements a ty)
  | Assign VarName (IExp ty)
  | AssignSub VarName (IExp ty) (IExp ty)
  | Decl VarName (Maybe (IExp ty))
  | SyncLocalMem
  | SyncGlobalMem
  | Allocate VarName (IExp ty) CType -- This should not be here, as
                                     -- allocated memory (local and
                                     -- global) should be passed as
                                     -- argument to kernels
                                     -- Just trying to emulate Obsidian for now
 deriving (Eq, Show)

data Kernel ty =
  Kernel { kernelName :: String
         , kernelParams :: [VarName]
         , kernelBody :: Statements () ty
         }
  deriving (Eq, Show)

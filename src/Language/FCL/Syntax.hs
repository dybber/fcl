module Language.FCL.Syntax where

type VarName = String

data UnaryOp = Not | I2D | NegateInt | NegateDouble |
               Ceil | Floor | Exp | Ln | AbsI | AbsD
  deriving (Eq, Show)

data BinOp   = AddI | SubI | MulI | DivI |
               AddD | SubD | MulD | DivD | 
               LtI | LteI | GtI | GteI | EqI | NeqI | 
               LtD | LteD | GtD | GteD | EqD | NeqD |
               And | Or
               -- | Land | Lor | Xor | Nand | Nor -- bitwise ops
  deriving (Eq, Show)

data Level = Workgroup
           | Local
           | Warp
           | Vec -- ^ For vectorized operations like "int4"
           | Sequential
  deriving (Eq, Show)

-- For untyped terms. They will have type "Exp NoType"
data NoType = NoType
  deriving (Eq, Show)

data Exp ty = 
    IntE Int
  | DoubleE Double
  | BoolE Bool
  | VarE VarName
  | UnaryOpE UnaryOp (Exp ty) ty
  | BinOpE BinOp (Exp ty) (Exp ty) ty
  | LetE VarName ty (Exp ty) (Exp ty)
  | IfE (Exp ty) (Exp ty) (Exp ty) ty
  | LamE VarName Type (Exp ty) ty
  | AppE (Exp ty) (Exp ty) ty
  | LamSizeE SizeVar (Exp ty) ty
  | AppSizeE (Exp ty) Size ty

--  | VectorE [Exp ty] ty -- ^ Array literals

  -- Do we need tuples, maybe only tuples of arrays?
  | UnitE
  | PairE (Exp ty) (Exp ty)
  | Proj1E (Exp ty)
  | Proj2E (Exp ty)

 -- ^ Built-in combinators
  | ReduceSeqE (Exp ty) (Exp ty) (Exp ty) ty -- TODO: Replace with general for-loops, like in Obsidian?
  | MapE Level (Exp ty) (Exp ty) ty

  | ToLocalE (Exp ty) ty
  | ToGlobalE (Exp ty) ty

  | ReorderE (Exp ty) ty
  | ReorderStrideE Size (Exp ty) ty

  | JoinE (Exp ty) ty
  | SplitE Size (Exp ty) ty
-- I will wait a bit before introducing these:
--  | IterateE Size (Exp ty) (Exp ty) ty
--  | ReducePartE (Exp ty) (Exp ty) (Exp ty) ty
 deriving (Eq, Show)

data Basetype = Int | Double | Bool

-- Size variable
newtype SizeVar = SVar String
  deriving (Eq, Show, Ord)

data Size = Size Int
          | SizeVar SizeVar
          | SizeProd Size Size
          | SizePow Size Size
  deriving (Eq, Show)

type TyVarName = Int

data Type = IntT
          | DoubleT
          | BoolT
          | UnitT -- ^ When and why do they need UnitT?
          | TyVar TyVarName
          | Type :*: Type
          | Type :> Type
          | SizeVar :=> Type
          | ArrayT Type Size
  deriving (Eq, Show)

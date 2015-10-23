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
  | VarE VarName ty
  | UnaryOpE UnaryOp (Exp ty)
  | BinOpE BinOp (Exp ty) (Exp ty)
  | LetE VarName TypeScheme (Exp ty) (Exp ty) ty
  | IfE (Exp ty) (Exp ty) (Exp ty) ty
  | LamE VarName ty (Exp ty) ty
  | AppE (Exp ty) (Exp ty) ty

  | VectorE [Exp ty] ty -- ^ Array literals

  -- Do we need tuples, maybe only tuples of arrays?
  | PairE (Exp ty) (Exp ty)
  | Proj1E (Exp ty)
  | Proj2E (Exp ty)

 -- ^ Built-in combinators
  | ReduceSeqE (Exp ty) (Exp ty) (Exp ty) ty -- TODO: Replace with general for-loops, like in Obsidian?
  | MapE Level (Exp ty) (Exp ty) ty

  | ToLocalE (Exp ty)
  | ToGlobalE (Exp ty)

  | ReorderE (Exp ty)
  | ReorderStrideE (Exp ty) (Exp ty)

  | JoinE (Exp ty) ty
  | SplitE (Exp ty) (Exp ty) ty
-- I will wait a bit before introducing these:
--  | IterateE (Exp ty) (Exp ty) (Exp ty) ty
--  | ReducePartE (Exp ty) (Exp ty) (Exp ty) ty
 deriving (Eq, Show)

data Basetype = Int | Double | Bool

type TyVarName = Int

data Type = IntT
          | DoubleT
          | BoolT
          | TyVar TyVarName
          | Type :*: Type
          | Type :> Type
          | ArrayT Type
  deriving (Eq, Show)

data TypeScheme = TypeScheme [TyVarName] Type
 deriving (Eq, Show)

typeOf :: Exp Type -> Type
typeOf (IntE _) = IntT
typeOf (DoubleE _) = DoubleT
typeOf (BoolE _) = BoolT
typeOf (VarE _ ty) = ty
typeOf (LetE _ _ _ _ ty) = ty
typeOf (LamE _ _ _ ty) = ty
typeOf (AppE _ _ ty) = ty
typeOf (UnaryOpE op _) =
  case op of
    Not          -> BoolT
    I2D          -> DoubleT
    NegateInt    -> IntT
    NegateDouble -> DoubleT
    Ceil         -> IntT
    Floor        -> IntT
    Exp          -> DoubleT
    Ln           -> DoubleT
    AbsI         -> IntT
    AbsD         -> DoubleT
typeOf (BinOpE op _ _) =
  if op `elem` [ AddI, SubI, MulI, DivI
               , LtI, LteI, GtI, EqI, NeqI]
    then IntT
    else
      if op `elem` [ AddD, SubD, MulD, DivD
                   , LtD, LteD, GtD, EqD, NeqD]
        then DoubleT
        else BoolT -- And, Or
typeOf (IfE _ _ _ ty) = ty
typeOf (VectorE _ ty) = ty
typeOf (PairE e1 e2) = typeOf e1 :*: typeOf e2
typeOf (Proj1E e) =
  case typeOf e of
    (t1 :*: _) -> t1
    _ -> error "typeOf: Expression not of product type"
typeOf (Proj2E e) =
  case typeOf e of
    (_ :*: t2) -> t2
    _ -> error "typeOf: Expression not of product type"
typeOf (ReduceSeqE _ _ _ ty) = ty
typeOf (MapE _ _ _ ty) = ty
typeOf (ToLocalE e) = typeOf e
typeOf (ToGlobalE e) = typeOf e
typeOf (ReorderE e) = typeOf e
typeOf (ReorderStrideE _ e) = typeOf e
typeOf (JoinE _ ty) = ty
typeOf (SplitE _ _ ty) = ty



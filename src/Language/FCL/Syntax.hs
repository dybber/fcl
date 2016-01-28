module Language.FCL.Syntax (
  Type(..),
  Exp(..),
  Level(..),
  UnOp(..),
  BinOp(..),
  Variable,
  typeOf,
  Untyped(Untyped),
  Definition(..),
  ExpEnv,
  Prog,
) where

import Language.GPUIL.Syntax (Level(..))

data UnOp = AbsI | SignI | NegateI
  deriving (Show, Eq)

data BinOp = AddI | SubI | MulI | DivI | ModI | MinI
           | EqI | NeqI | AndI | XorI | ShiftLI | ShiftRI
  deriving (Eq, Show)

type Variable = String

data Untyped = Untyped
  deriving (Eq, Show)

data Type =
    IntT
  | BoolT
  | DoubleT
  | TyVar String
  | Type :> Type
  | Type :*: Type
  | ArrayT Level Type
 deriving (Eq, Show)

type Prog ty = [Definition ty]

type ExpEnv = [(Variable, Exp Type)]

data Definition ty = Definition Variable (Maybe Type) (Exp ty)
                   | KernelDef Variable
 deriving Show

data Exp ty =
    IntScalar Int
  | DoubleScalar Double
  | BoolScalar Bool
  | UnOp UnOp (Exp ty)
  | BinOp BinOp (Exp ty) (Exp ty)
  | Var Variable ty
  | Vec [Exp ty] ty
  | Lamb Variable Type (Exp ty) ty
  | Let Variable (Exp ty) (Exp ty) ty
  | App (Exp ty) (Exp ty)
  | Cond (Exp ty) (Exp ty) (Exp ty) ty
  | Pair (Exp ty) (Exp ty)
  | Proj1E (Exp ty)
  | Proj2E (Exp ty)

-- Array handling
  | Index (Exp ty) (Exp ty)
  | Length (Exp ty) -- array length

-- Combinators
  | Fixpoint (Exp ty) (Exp ty) (Exp ty) -- APL-style, representing tail-recursive functions
  | Generate Level (Exp ty) (Exp ty) -- mkPull
  | Map (Exp ty) (Exp ty)
  | ForceLocal (Exp ty) -- force
  | Concat (Exp ty) (Exp ty)
  | Assemble (Exp ty) (Exp ty) (Exp ty)
  deriving (Eq, Show)

  -- To be added later!
  -- | Replicate Exp Exp
  -- | Permute Exp Exp
  -- | Append Exp Exp

typeOf :: Exp Type -> Type
typeOf (IntScalar _) = IntT
typeOf (DoubleScalar _) = DoubleT
typeOf (BoolScalar _) = BoolT
typeOf (UnOp op _) =
    if op `elem` [ AbsI, SignI ]
    then IntT
    else error "typeOf: UnOp"
typeOf (BinOp op _ _) =
  if op `elem` [ AddI, SubI, MulI, DivI, ModI, MinI ]
    then IntT
    else
      if op `elem` [ EqI ]
        then BoolT
        else error "typeOf: BinOp"
typeOf (Var _ ty) = ty
typeOf (Lamb _ ty0 _ ty1) = ty0 :> ty1
typeOf (Let _ _ _ ty) = ty
typeOf (App e _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (Cond _ _ _ ty) = ty
typeOf (Pair e0 e1) = (typeOf e0) :*: (typeOf e1)
typeOf (Proj1E e) =
  case typeOf e of
    (t0 :*: _) -> t0
    _ -> error "typeOf: Argument to Proj1E not of product type"
typeOf (Proj2E e) =
  case typeOf e of
    (_ :*: t1) -> t1
    _ -> error "typeOf: Argument to Proj2E not of product type"
typeOf (Index e0 _) =
  case typeOf e0 of
    ArrayT _ elem_ty -> elem_ty
    _ -> error "typeOf: Argument to Index not of array type"
typeOf (Length _) = IntT
typeOf (Fixpoint _ _ e2) = typeOf e2
typeOf (Generate lvl _ f) =
  case typeOf f of
    _ :> ty1 -> ArrayT lvl ty1
    _ -> error "typeOf: Second argument to Generate not of function type"
typeOf (Map e0 e1) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, ArrayT lvl _) -> ArrayT lvl ty1
    _ -> error "typeOf: Map"
typeOf (ForceLocal e0) = typeOf e0
typeOf (Concat _ e0) =
  case typeOf e0 of
    ArrayT _ t -> t
    _ -> error "typeOf: Concat given non-array as third argument"
typeOf (Assemble _ _ e0) =
  case typeOf e0 of
    ArrayT _ t -> t
    _ -> error "typeOf: Assemble given non-array as third argument"
typeOf (Vec [] _) = error "Cannot type empty list"
typeOf (Vec es _) =
  let (t:ts) = map typeOf es
  in if and $ zipWith (==) (t:ts) ts
       then ArrayT undefined t
       else error ""

module Language.FCL.Syntax (
  -- Types
  Level(..),
  Type(..),
  TyVar(..),
  TypeScheme(..),
  freevars,
  Untyped(Untyped),

  -- Expressions
  Variable,
  Exp(..),
  UnOp(..),
  BinOp(..),
  typeOf,

  -- Programs
  Definition(..),
  Program,
  mapBody
) where

import Language.FCL.SourceRegion

---------------------
-- Syntax of types --
---------------------
data Level = Thread | Warp | Block | Grid
  deriving (Eq, Show)
       
data Type =
    IntT
  | BoolT
  | DoubleT
  | VarT TyVar
  | Type :> Type
  | Type :*: Type
  | ArrayT Level Type
  deriving (Eq, Show)

data TyVar = TyVar Int (Maybe String)
  deriving (Eq, Show, Ord)

data TypeScheme ty = TypeScheme [TyVar] ty
  deriving Show

-- | Return the list of type variables in t (possibly with duplicates)
freevars :: Type -> [TyVar]
freevars IntT       = []
freevars BoolT       = []
freevars DoubleT       = []
freevars (t1 :> t2) = freevars t1 ++ freevars t2
freevars (t1 :*: t2) = freevars t1 ++ freevars t2
freevars (ArrayT Block t) = freevars t
freevars (ArrayT _ _) = error "Only block level allowed ATM" -- TODO
freevars (VarT v)  = [v]

data Untyped = Untyped
  deriving (Eq, Show)

---------------------------
-- Syntax of Expressions --
---------------------------
type Variable = String


data Exp ty =
    IntScalar Int Region
  | DoubleScalar Double Region
  | BoolScalar Bool Region
  | UnOp UnOp (Exp ty) Region
  | BinOp BinOp (Exp ty) (Exp ty) Region
  | Var Variable ty Region
  | Vec [Exp ty] ty Region
  | Lamb Variable ty (Exp ty) ty Region
  | Let Variable (Exp ty) (Exp ty) ty Region
  | App (Exp ty) (Exp ty)
  | Cond (Exp ty) (Exp ty) (Exp ty) ty Region
  | Pair (Exp ty) (Exp ty) Region
  | Proj1E (Exp ty) Region
  | Proj2E (Exp ty) Region

-- Array handling
  | Index (Exp ty) (Exp ty) Region
  | Length (Exp ty) Region
-- Combinators
  | While (Exp ty) (Exp ty) (Exp ty) Region -- APL-style, representing tail-recursive functions
  | Generate Level (Exp ty) (Exp ty) Region
  | Map (Exp ty) (Exp ty) Region
  | ForceLocal (Exp ty) Region
  | Concat (Exp ty) (Exp ty) Region
  -- | Assemble (Exp ty) (Exp ty) (Exp ty)
  | LocalSize Region

  -- Sequential scan, I don't really want this!
  | Scanl (Exp ty) (Exp ty) (Exp ty) Region
  deriving (Eq, Show)

  -- To be added later!
  -- | Replicate Exp Exp
  -- | Permute Exp Exp
  -- | Append Exp Exp

data UnOp = AbsI | SignI | NegateI | Not | I2D
  deriving (Eq, Show)

data BinOp = AddI | SubI | MulI | DivI | ModI | MinI
           | EqI | NeqI | AndI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
  deriving (Eq, Show)


typeOf :: Exp Type -> Type
typeOf (IntScalar _ _) = IntT
typeOf (DoubleScalar _ _) = DoubleT
typeOf (BoolScalar _ _) = BoolT
typeOf (UnOp op _ _) =
    if op `elem` [ AbsI, SignI ]
    then IntT
    else error "typeOf: UnOp"
typeOf (BinOp op _ _ _) =
  if op `elem` [ AddI, SubI, MulI, DivI, ModI, MinI ]
    then IntT
    else
      if op `elem` [ EqI ]
        then BoolT
        else error "typeOf: BinOp"
typeOf (Var _ ty _) = ty
typeOf (Lamb _ ty0 _ ty1 _) = ty0 :> ty1
typeOf (Let _ _ _ ty _) = ty
typeOf (App e _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (Cond _ _ _ ty _) = ty
typeOf (Pair e0 e1 _) = (typeOf e0) :*: (typeOf e1)
typeOf (Proj1E e _) =
  case typeOf e of
    (t0 :*: _) -> t0
    _ -> error "typeOf: Argument to Proj1E not of product type"
typeOf (Proj2E e _) =
  case typeOf e of
    (_ :*: t1) -> t1
    _ -> error "typeOf: Argument to Proj2E not of product type"
typeOf (Index e0 _ _) =
  case typeOf e0 of
    ArrayT _ elem_ty -> elem_ty
    _ -> error "typeOf: Argument to Index not of array type"
typeOf (Length _ _) = IntT
typeOf (While _ _ e2 _) = typeOf e2
typeOf (Generate lvl _ f _) =
  case typeOf f of
    _ :> ty1 -> ArrayT lvl ty1
    _ -> error "typeOf: Second argument to Generate not of function type"
typeOf (Map e0 e1 _) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, ArrayT lvl _) -> ArrayT lvl ty1
    _ -> error "typeOf: Map"
typeOf (ForceLocal e0 _) = typeOf e0
typeOf (Concat _ e0 _) =
  case typeOf e0 of
    ArrayT _ t -> t
    _ -> error "typeOf: Concat given non-array as third argument"
-- typeOf (Assemble _ _ e0) =
--   case typeOf e0 of
--     ArrayT _ t -> t
--     _ -> error "typeOf: Assemble given non-array as third argument"
typeOf (Vec [] _ _) = error "Cannot type empty list"
typeOf (Vec es _ _) =
  let (t:ts) = map typeOf es
  in if and $ zipWith (==) (t:ts) ts
       then ArrayT undefined t
       else error ""
typeOf (LocalSize _) = IntT
typeOf (Scanl _ e1 e2 _) =
  case typeOf e2 of
    ArrayT lvl _ -> ArrayT lvl (typeOf e1)
    _ -> error "typeOf: Scanl"


------------------------
-- Syntax of programs --
------------------------

type Program ty = [Definition ty]
data Definition ty =
  Definition
    { defVar        :: Variable
    , defSignature  :: Maybe Type
    , defTypeScheme :: TypeScheme ty
    , defEmitKernel :: Bool
    , defBody       :: Exp ty
    }
  deriving Show

mapBody :: (Exp ty -> Exp ty)
        -> Definition ty -> Definition ty
mapBody f def = def { defBody = f (defBody def) }

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
    IntScalar Int
  | DoubleScalar Double
  | BoolScalar Bool
  | UnOp UnOp (Exp ty)
  | BinOp BinOp (Exp ty) (Exp ty)
  | Var Variable ty
  | Vec [Exp ty] ty
  | Lamb Variable ty (Exp ty) ty
  | Let Variable (Exp ty) (Exp ty) ty
  | App (Exp ty) (Exp ty)
  | Cond (Exp ty) (Exp ty) (Exp ty) ty
  | Pair (Exp ty) (Exp ty)
  | Proj1E (Exp ty)
  | Proj2E (Exp ty)

-- Array handling
  | Index (Exp ty) (Exp ty)
  | Length (Exp ty)
-- Combinators
  | Fixpoint (Exp ty) (Exp ty) (Exp ty) -- APL-style, representing tail-recursive functions
  | Generate Level (Exp ty) (Exp ty)
  | Map (Exp ty) (Exp ty)
  | ForceLocal (Exp ty)
  | Assemble (Exp ty) (Exp ty) (Exp ty)
  | LocalSize
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
-- typeOf (Concat _ e0) =
--   case typeOf e0 of
--     ArrayT _ t -> t
--     _ -> error "typeOf: Concat given non-array as third argument"
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
typeOf LocalSize = IntT


------------------------
-- Syntax of programs --
------------------------
-- type UntypedProgram ty = [Definition ty]
-- data Definition ty = Definition Variable (Maybe Type) Bool (Exp ty)
--   deriving Show

-- type TypedProgram = [TypedDef]
-- data TypedDef = TypedDef Variable Bool TypeScheme (Exp Type)

-- type Program = [(Variable, Exp Type)]

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

mapBody :: (Exp ty -> Exp ty) ->
           Definition ty -> Definition ty
mapBody f def = def { defBody = f (defBody def) }

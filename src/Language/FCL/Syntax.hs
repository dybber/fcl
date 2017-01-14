-- This module containts the FCL AST
module Language.FCL.Syntax (
  -- Levels
  Level(..), threadLevel, blockLevel, gridLevel, LvlVar(..),
  
  -- Types
  Type(..), TyVar(..), TypeScheme(..), Untyped(Untyped),
  
  -- Expressions
  Literal(..),
  Exp(..),
  typeOf,
  isLiteral,
  
  -- Operators
  UnaryOperator(..),
  BinaryOperator(..),

  -- Programs
  Definition(..),
  mapBody
) where

import Language.FCL.Identifier
import Language.FCL.SourceRegion

---------------------
-- Syntax of types --
---------------------
data LvlVar = LvlVar Int (Maybe Identifier)
  deriving (Eq, Show, Ord)

threadLevel, blockLevel, gridLevel :: Level
threadLevel = Zero
blockLevel  = Step threadLevel
gridLevel   = Step blockLevel

data Level = Zero | Step Level | VarL LvlVar
  deriving (Eq, Show)
           
data Type =
    IntT
  | BoolT
  | DoubleT
  | StringT
  | UnitT
  | VarT TyVar
  | LvlVar :-> Type
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  | ProgramT Level Type
  deriving (Eq, Show)

data TyVar = TyVar Int (Maybe Identifier)
  deriving (Eq, Show, Ord)

data TypeScheme ty = TypeScheme [TyVar] ty
  deriving (Eq, Show)

data Untyped = Untyped
  deriving (Eq, Show)

---------------------------
-- Syntax of Expressions --
---------------------------
data Literal = LiteralInt Int
             | LiteralDouble Double
             | LiteralBool Bool
             | LiteralString String
             | Unit
  deriving (Show, Eq, Ord)

data Exp ty =
    Literal Literal SourceRegion
  | UnaryOp UnaryOperator (Exp ty) SourceRegion
  | BinaryOp BinaryOperator (Exp ty) (Exp ty) SourceRegion
  | Var Identifier ty SourceRegion
  | Vec [Exp ty] ty SourceRegion
  | Lamb Identifier ty (Exp ty) ty SourceRegion
  | Let Identifier (Exp ty) (Exp ty) ty SourceRegion
  | App (Exp ty) (Exp ty)

  | LambLvl LvlVar (Exp ty) ty SourceRegion
  | AppLvl (Exp ty) Level
    
  | Cond (Exp ty) (Exp ty) (Exp ty) ty SourceRegion
  | Pair (Exp ty) (Exp ty) SourceRegion
  | Proj1E (Exp ty) SourceRegion
  | Proj2E (Exp ty) SourceRegion

-- Array handling
  | Index (Exp ty) (Exp ty) SourceRegion
  | LengthPull (Exp ty) SourceRegion
  | LengthPush (Exp ty) SourceRegion

-- Combinators
  | For (Exp ty) (Exp ty) (Exp ty) SourceRegion -- Sequential for loop at thread-level
  | Power (Exp ty) (Exp ty) (Exp ty) SourceRegion -- APL-style power-operator
  | While (Exp ty) (Exp ty) (Exp ty) SourceRegion -- APL-style, representing tail-recursive functions
  | WhileSeq (Exp ty) (Exp ty) (Exp ty) SourceRegion
  | GeneratePull (Exp ty) (Exp ty) SourceRegion
  | MapPull (Exp ty) (Exp ty) SourceRegion
  | MapPush (Exp ty) (Exp ty) SourceRegion
  | Force (Exp ty) SourceRegion
  | Push Level (Exp ty) SourceRegion
  | Interleave (Exp ty) (Exp ty) (Exp ty) SourceRegion
  | Bind (Exp ty) (Exp ty) SourceRegion
  | Return Level (Exp ty) SourceRegion
  | BlockSize SourceRegion

-- I/O
  | ReadIntCSV (Exp ty) SourceRegion
  -- | ReadDoubleCSV (Exp ty)
  | ForceAndPrint (Exp ty) (Exp ty) SourceRegion
  | Benchmark (Exp ty) (Exp ty) SourceRegion
  -- | PrintDoubleArray (Exp ty) (Exp ty)
  deriving Show

data UnaryOperator = AbsI | SignI | NegateI | Not | I2D | B2I | CLZ
  deriving (Eq, Show)

data BinaryOperator = AddI | SubI | MulI | DivI | ModI | MinI | MaxI
           | EqI | NeqI | LtI | AndI | OrI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
           | AddR
  deriving (Eq, Show)

isLiteral :: Exp ty -> Bool
isLiteral (Literal _ _) = True
isLiteral _ = False

typeOf :: Exp Type -> Type
typeOf (Literal (LiteralInt _) _) = IntT
typeOf (Literal (LiteralDouble _) _) = DoubleT
typeOf (Literal (LiteralBool _) _) = BoolT
typeOf (Literal (LiteralString _) _) = StringT
typeOf (Literal Unit _) = UnitT
typeOf (UnaryOp op _ _) =
    if op `elem` [ AbsI, SignI ]
    then IntT
    else error "typeOf: UnOp"
typeOf (BinaryOp op _ _ _) =
  if op `elem` [ AddI, SubI, MulI, DivI, ModI, MinI ]
    then IntT
    else
      if op `elem` [ EqI ]
        then BoolT
        else error "typeOf: BinOp"
typeOf (Var _ ty _) = ty
typeOf (Lamb _ ty0 _ ty1 _) = ty0 :> ty1
typeOf (App e _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (LambLvl lvlvar _ ty _) = lvlvar :-> ty
typeOf (AppLvl e _) =
  case typeOf e of
    _ :-> ty -> ty
    _ -> error "typeOf: First argument to AppLvl not of function type"
typeOf (Let _ _ _ ty _) = ty
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
    PullArrayT elem_ty -> elem_ty
    _ -> error "typeOf: Argument to Index not of push-array type"
typeOf (LengthPull _ _) = IntT
typeOf (LengthPush _ _) = IntT
typeOf (Power _ _ e2 _) =
  case typeOf e2 of
    (ProgramT lvl (PushArrayT _ ty)) -> ProgramT lvl (PullArrayT ty)
    _ -> error "typeOf: Argument to Power not of Program lvl push-array type."
typeOf (For _ _ e2 _) =
  case typeOf e2 of
    ((_ :> ty) :> _) -> ty
    _ -> error "typeOf: Argument to For not of correct type."
typeOf (While _ _ e2 _) = typeOf e2
typeOf (WhileSeq _ _ e2 _) = typeOf e2
typeOf (GeneratePull _ f _) =
  case typeOf f of
    _ :> ty1 -> PullArrayT ty1
    _ -> error "typeOf: Second argument to Generate not of function type"
typeOf (MapPull e0 e1 _) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, PullArrayT _) -> PullArrayT ty1
    _ -> error "typeOf: Map"
typeOf (MapPush e0 e1 _) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, PushArrayT lvl _) -> PushArrayT lvl ty1
    _ -> error "typeOf: Map"
typeOf (Push lvl e0 _) =
  case typeOf e0 of
    PullArrayT ty -> PushArrayT lvl ty
    _ -> error "typeOf: push"
typeOf (Force e0 _) =
  case (typeOf e0) of
    (PushArrayT _ ty0) -> PullArrayT ty0
    _ -> error "typeOf: Force"
typeOf (Interleave _ _ e0 _) =
  case typeOf e0 of
    PullArrayT (PushArrayT lvl t) -> PushArrayT (Step lvl) t
    _ -> error "typeOf: Interleave given non-pull-array as third argument"
typeOf (Vec [] _ _) = error "Cannot type empty list"
typeOf (Vec es _ _) =
  let (t:ts) = map typeOf es
  in if and $ zipWith (==) (t:ts) ts
       then PullArrayT t
       else error "All elements in vector literal should be typed identically"
typeOf (BlockSize _) = IntT
typeOf (Return lvl e0 _) =
  ProgramT lvl (typeOf e0)
typeOf (Bind _ e1 _) =
  case typeOf e1 of
    (_ :> ty) -> ty
    _ -> error "typeOf: bind"
typeOf (ReadIntCSV _ _) = ProgramT gridLevel (PullArrayT StringT)
typeOf (ForceAndPrint _ _ _) = ProgramT gridLevel UnitT
typeOf (Benchmark _ _ _) = ProgramT gridLevel UnitT

------------------------
-- Syntax of programs --
------------------------

data Definition ty =
  Definition
    { defVar        :: Identifier
    , defSignature  :: Maybe Type
    , defTypeScheme :: TypeScheme ty
    , defBody       :: Exp ty
    }
  deriving Show

mapBody :: (Exp ty -> Exp ty)
        -> Definition ty -> Definition ty
mapBody f def = def { defBody = f (defBody def) }

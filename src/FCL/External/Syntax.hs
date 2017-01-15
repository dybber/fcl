-- This module containts the FCL AST
module FCL.External.Syntax (
  -- Levels
  Level(..), LvlVar(..), threadLevel, blockLevel, gridLevel,
  
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

import FCL.Core.Identifier
import FCL.Core.SourceRegion

---------------------
-- Syntax of types --
---------------------
data LvlVar = LvlVar Int (Maybe Identifier)
  deriving (Eq, Show, Ord)

data Level = Zero | Step Level | VarL LvlVar
  deriving (Eq, Show)

threadLevel, blockLevel, gridLevel :: Level
threadLevel = Zero
blockLevel  = Step threadLevel
gridLevel   = Step blockLevel

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

infixr 9 :>
infixr 9 :->

data TyVar = TyVar Int (Maybe Identifier)
  deriving (Eq, Show, Ord)

data TypeScheme ty = TypeScheme [TyVar] [LvlVar] ty
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
  | Symbol Identifier ty SourceRegion
  | Vec [Exp ty] ty SourceRegion
  | Lamb Identifier ty (Exp ty) ty SourceRegion
  | Let Identifier (Exp ty) (Exp ty) ty SourceRegion
  | App (Exp ty) (Exp ty) SourceRegion

  | LambLvl LvlVar (Exp ty) ty SourceRegion
  | AppLvl (Exp ty) Level SourceRegion
    
  | Cond (Exp ty) (Exp ty) (Exp ty) ty SourceRegion
  | Pair (Exp ty) (Exp ty) SourceRegion

  | UnaryOp UnaryOperator (Exp ty) SourceRegion
  | BinaryOp BinaryOperator (Exp ty) (Exp ty) SourceRegion

--   | Proj1E (Exp ty) SourceRegion
--   | Proj2E (Exp ty) SourceRegion

-- -- Array handling
--   | Index (Exp ty) (Exp ty) SourceRegion
--   | LengthPull (Exp ty) SourceRegion
--   | LengthPush (Exp ty) SourceRegion

-- -- Combinators
--   | For (Exp ty) (Exp ty) (Exp ty) SourceRegion -- Sequential for loop at thread-level
--   | Power (Exp ty) (Exp ty) (Exp ty) SourceRegion -- APL-style power-operator
--   | While (Exp ty) (Exp ty) (Exp ty) SourceRegion -- APL-style, representing tail-recursive functions
--   | WhileSeq (Exp ty) (Exp ty) (Exp ty) SourceRegion
--   | GeneratePull (Exp ty) (Exp ty) SourceRegion
--   | MapPull (Exp ty) (Exp ty) SourceRegion
--   | MapPush (Exp ty) (Exp ty) SourceRegion
--   | Force (Exp ty) SourceRegion
--   | Push Level (Exp ty) SourceRegion
--   | Interleave (Exp ty) (Exp ty) (Exp ty) SourceRegion
--   | Bind (Exp ty) (Exp ty) SourceRegion
--   | Return Level (Exp ty) SourceRegion
--   | BlockSize SourceRegion

-- -- I/O
--   | ReadIntCSV (Exp ty) SourceRegion
--   -- | ReadDoubleCSV (Exp ty)
--   | ForceAndPrint (Exp ty) (Exp ty) SourceRegion
--   | Benchmark (Exp ty) (Exp ty) SourceRegion
--   -- | PrintDoubleArray (Exp ty) (Exp ty)
  deriving Show

data UnaryOperator = AbsI | SignI | NegateI | Not | B2I | CLZ
  deriving (Eq, Show)

data BinaryOperator = AddI | SubI | MulI | DivI | ModI
           | EqI | NeqI | AndI | OrI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
           | AddR
  deriving (Eq, Show)

isLiteral :: Exp ty -> Bool
isLiteral (Literal _ _) = True
isLiteral _ = False

instance SourceInfo (Exp ty) where
  getSourceRegion e =
    case e of
      Literal _ r      -> r
      Symbol _ _ r     -> r
      Vec _ _ r        -> r
      Lamb _ _ _ _ r   -> r
      Let _ _ _ _ r    -> r
      App _ _ r        -> r

      LambLvl _ _ _ r  -> r
      AppLvl _ _ r     -> r

      Cond _ _ _ _ r   -> r
      Pair _ _ r       -> r

      UnaryOp _ _ r    -> r
      BinaryOp _ _ _ r -> r

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
  if op `elem` [ AddI, SubI, MulI, DivI, ModI ]
    then IntT
    else
      if op `elem` [ EqI ]
        then BoolT
        else error "typeOf: BinOp"
typeOf (Symbol _ ty _) = ty
typeOf (Lamb _ ty0 _ ty1 _) = ty0 :> ty1
typeOf (App e _ _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (LambLvl lvlvar _ ty _) = lvlvar :-> ty
typeOf (AppLvl e _ _) =
  case typeOf e of
    _ :-> ty -> ty
    _ -> error "typeOf: First argument to AppLvl not of function type"
typeOf (Let _ _ _ ty _) = ty
typeOf (Cond _ _ _ ty _) = ty
typeOf (Pair e0 e1 _) = (typeOf e0) :*: (typeOf e1)
typeOf (Vec [] _ _) = error "Cannot type empty list"
typeOf (Vec es _ _) =
  let (t:ts) = map typeOf es
  in if and $ zipWith (==) (t:ts) ts
       then PullArrayT t
       else error "All elements in vector literal should be typed identically"

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

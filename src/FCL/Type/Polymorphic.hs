module FCL.Type.Polymorphic where

import FCL.Core.Identifier

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

module Language.ObsidianLight.Syntax (
  OType(..),
  OExp(..),
  Level(..),
  BinOp(..),
  VarName
)
where

import Language.GPUIL.Syntax (Level(..))

data BinOp = AddI | SubI | MulI | DivI | ModI | MinI | EqI
  deriving (Eq, Show)

type VarName = String

data OType =
    IntT
  | BoolT
  | DoubleT
  | FunT OType OType
  | ProductT OType OType
  | Array Level OType

data OExp =
    IntScalar Int
  | DoubleScalar Double
  | BoolScalar Bool
  | BinOp BinOp OExp OExp

  | Var VarName
  | Lamb VarName OExp
  | Let VarName OExp OExp
  | App OExp OExp
  | Cond OExp OExp OExp
  | Pair OExp OExp
  | Proj1E OExp
  | Proj2E OExp

-- Array handling
  | Index OExp OExp
  | Length OExp -- array length

-- Combinators
  | Fixpoint OExp OExp OExp -- APL-style, representing tail-recursive functions
  | Generate Level OExp OExp -- mkPull
  | Map OExp OExp
  | ForceLocal OExp -- force
  deriving (Eq, Show)

  -- To be added later!
  -- | Iota OExp
  -- | Replicate OExp OExp
  -- | Permute OExp OExp
  -- | Append OExp OExp

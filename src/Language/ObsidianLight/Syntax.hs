module Language.ObsidianLight.Syntax where

data Level = Block | Warp | Thread

data BinOp = AddI | SubI | DivI | EqI | MinI

type VarName = String

data OExp =
    IntScalar Int
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
  | ComputeLocal OExp -- force

  -- To be added later!
  -- | Iota OExp
  -- | Replicate OExp OExp
  -- | Permute OExp OExp
  -- | Append OExp OExp

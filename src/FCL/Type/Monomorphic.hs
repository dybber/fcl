module FCL.Type.Monomorphic where

---------------------
-- Syntax of types --
---------------------
data Level = Thread | Block | Grid
  deriving (Eq, Show)

data Type =
    IntT
  | BoolT
  | DoubleT
  | StringT
  | UnitT
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  | ProgramT Level Type
  deriving (Eq, Show)

infixr 9 :>

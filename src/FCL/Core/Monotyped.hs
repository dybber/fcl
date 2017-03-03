module FCL.Core.Monotyped where

import FCL.Core.Identifier
import FCL.Core.MonoLevel
import FCL.Core.Literal

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

data Exp =
    Literal Literal Type
  | Unit Type
  | Symbol Identifier Type
  | Lamb Identifier Exp Type
  | App Exp Exp Type
  | Pair Exp Exp Type
  | Cond Exp Exp Exp Type
  | Let Identifier Exp Exp Type
  deriving Show

isLiteral :: Exp -> Bool
isLiteral (Literal _ _) = True
isLiteral _ = False

typeOf :: Exp -> Type
typeOf l =
  case l of
    (Literal _ ty)  -> ty
    (Unit ty)       -> ty
    (Symbol _ ty)   -> ty
    (Lamb _ _ ty)   -> ty
    (App _ _ ty)    -> ty
    (Pair _ _ ty)   -> ty
    (Cond _ _ _ ty) -> ty
    (Let _ _ _ ty)  -> ty

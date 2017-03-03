module FCL.Core.Untyped where

import FCL.Core.Identifier
import FCL.Core.PolyLevel
import FCL.Core.Literal
import FCL.Core.Polytyped (TypeScheme(..))

data Exp =
    Literal Literal
  | Unit
  | Symbol Identifier [Level]
  | Lamb Identifier Exp
  | App Exp Exp
  | Pair Exp Exp
  | Cond Exp Exp Exp
  | Let Identifier (Maybe TypeScheme) [LvlVar] Exp Exp
  deriving Show

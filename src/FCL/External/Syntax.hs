module FCL.External.Syntax where

import FCL.Core.Identifier
import FCL.Core.Literal

data Level =
    Zero
  | Thread
  | Block
  | Grid
  | Step Level
  | VarL LvlVar
 deriving (Eq, Ord, Show)

data Type =
    IntT
  | BoolT
  | DoubleT
  | StringT
  | UnitT
  | VarT TyVar
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  | ProgramT Level Type
  deriving (Eq, Ord, Show)

infixr 9 :>

newtype LvlVar = LvlVar Identifier
  deriving (Eq, Ord, Show)

newtype TyVar = TyVar Identifier
  deriving (Eq, Ord, Show)

data TypeScheme = TypeScheme [TyVar] [LvlVar] Type
  deriving (Eq, Ord, Show)

data Exp =
    Literal Literal
  | Unit
  | Symbol Identifier [Level]
  | Lamb Identifier Exp
  | App Exp Exp
  | Pair Exp Exp
  | Cond Exp Exp Exp
  | Let Identifier (Maybe TypeScheme) [LvlVar] Exp Exp
  | UnaryOp UnaryOperator Exp
  | BinaryOp BinaryOperator Exp Exp
  | Do Level [DoStmt]
  deriving Show

data DoStmt =
    DoExp Exp
  | DoBind Identifier Exp
  deriving Show

data UnaryOperator = AbsI | SignI | NegateI | Not | B2I | CLZ
  deriving (Eq, Show)

data BinaryOperator = AddI | SubI | MulI | DivI | ModI
           | EqI | NeqI | AndI | OrI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
           | AddR
  deriving (Eq, Show)

------------------------
-- Syntax of programs --
------------------------
data FunctionDefinition =
  FunctionDefinition
    { funName                     :: Identifier
    , funSignature                :: Maybe TypeScheme
    , funQuantifiedLevelVariables :: [LvlVar]
    , funParameters               :: [Identifier]
    -- , defSignature           :: Maybe Type
    , funBody                     :: Exp
    }
  deriving Show

-- A program is a list of declarations where
-- one of them must be "main"
type Program = [FunctionDefinition]

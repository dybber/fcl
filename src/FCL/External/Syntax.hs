module FCL.External.Syntax
  (Literal(..),
   Level(..),
   LvlVar(..),
   Type(..),
   TyVar(..),
   TypeScheme(..),
   Exp(..),
   DoStmt(..),
   UnaryOperator(..),
   BinaryOperator(..),
   FunctionDefinition(..),
   Program(..),
   concatPrograms)
where

import FCL.Core.Identifier
import FCL.Core.PolyLevel
import FCL.Core.Polytyped (Type(..), TyVar(..), TypeScheme(..))
import FCL.Core.Literal

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

data UnaryOperator = Not
  deriving (Eq, Show)

data BinaryOperator = AddI | SubI | MulI | DivI | ModI
           | EqI | NeqI | AndI | OrI | XorI | ShiftLI | ShiftRI
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
    , funBody                     :: Exp
    }
  deriving Show

-- A program is a list of declarations where
-- one of them must be "main"
newtype Program = Program [FunctionDefinition]

concatPrograms :: [Program] -> Program
concatPrograms =
  let unProgram (Program p) = p
  in Program . concat . map unProgram

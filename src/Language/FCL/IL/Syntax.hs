module Language.FCL.IL.Syntax where

import Data.Set as Set

type ILName = (String, ILType)

data ILLevel = Thread | Block | Grid
  deriving Show
data ILType = ILInt | ILDouble | ILBool | ILString | ILArray ILType
  deriving (Ord, Eq, Show)

data ILExp =
    EInt Int
  | EDouble Double
  | EBool Bool
  | EString String
  | EVar ILName
  | EIndex ILName ILExp
  | EUnaryOp UnaryOp ILExp
  | EBinOp BinOp ILExp ILExp
  | EIf ILExp ILExp ILExp

  -- | EAlloc ILType ILExp
  -- | EReadIntCSV ILExp
  -- | EReadDoubleCSV ILExp
  -- | RandomIntVector VarILName ILExp
  -- | RandomDoubleVector VarILName ILExp
  deriving Show

data BinOp =
    AddI | SubI | MulI | DivI | ModI |
    AddD | SubD | MulD | DivD |
    LtI | LteI | GtI | GteI | EqI | NeqI | 
    LtD | LteD | GtD | GteD | EqD | NeqD |
    And | Or |
    Land | Lor | Xor | Sll | Srl | -- bitwise ops
    MinI | MaxI
  deriving (Eq, Show, Ord)

data UnaryOp = SignI | AbsI | AbsD
  deriving Show

data Stmt =
    Declare ILName ILExp
  | Alloc ILName ILType ILExp
  | Distribute ILLevel ILName ILExp [Stmt]
  | ParFor ILLevel ILName ILExp [Stmt]
  | Synchronize
  | Assign ILName ILExp
  | AssignSub ILName ILExp ILExp
  | If ILExp [Stmt] [Stmt]
  | While ILExp [Stmt]
  | SeqFor ILName ILExp [Stmt]
  | ReadIntCSV ILName ILName ILExp
  | PrintIntArray ILExp ILExp
  | Benchmark ILExp [Stmt]
 deriving Show

type ILProgram = [Stmt]

liveInExp :: ILExp -> Set ILName
liveInExp e =
  case e of
    EVar name           -> Set.singleton name
    -- Recursive
    EUnaryOp _ e0       -> liveInExp e0
    EBinOp _ e0 e1      -> liveInExp e0 `Set.union` liveInExp e1
    EIf e0 e1 e2        -> liveInExp e0 `Set.union` liveInExp e1 `Set.union` liveInExp e2
    EIndex name e0      -> Set.insert name (liveInExp e0)
    -- Scalars and constants
    EInt _              -> Set.empty
    EDouble _           -> Set.empty
    EBool _             -> Set.empty
    EString _           -> Set.empty

freeVars :: [Stmt] -> Set ILName
freeVars stmts =
  let
    freeInExp :: Set ILName -> ILExp -> Set ILName
    freeInExp bound e = liveInExp e `difference` bound

    fv :: Set ILName -> [Stmt] -> Set ILName
    fv _ [] = Set.empty
    -- binding forms
    fv bound (Declare x e : ss) = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (Alloc x _ e : ss) = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (ReadIntCSV x xlen e : ss) = freeInExp bound e `union` fv (insert xlen (insert x bound)) ss
    -- loops
    fv bound (Distribute _ x e body : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    fv bound (ParFor _ x e body : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    fv bound (While e body : ss) =
        freeInExp bound e
         `union` fv bound body
         `union` fv bound ss
    fv bound (SeqFor x e body : ss) =
        freeInExp bound e
         `union` fv (insert x bound) body
         `union` fv bound ss
    -- other
    fv bound (Synchronize : ss) = fv bound ss
    fv bound (Assign x e : ss) = (insert x (liveInExp e) `difference` bound) `union` fv bound ss
    fv bound (AssignSub x e0 e1 : ss) = ((insert x (liveInExp e0) `union` liveInExp e1) `difference` bound) `union` fv bound ss
    fv bound (If e ss0 ss1 : ss) =
      freeInExp bound e
      `union` fv bound ss0
      `union` fv bound ss1
      `union` fv bound ss
    fv bound (PrintIntArray e0 e1 : ss) = freeInExp bound e0 `union` freeInExp bound e1 `union` fv bound ss
    fv bound (Benchmark e ss0 : ss) =
      freeInExp bound e
       `union` fv bound ss0
       `union` fv bound ss
    
  in fv Set.empty stmts

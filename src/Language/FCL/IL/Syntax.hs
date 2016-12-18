module Language.FCL.IL.Syntax where

import Data.Set as Set

type Name = (String, ILType)

data Level = Thread | Block | Grid

data ILType = IntT | BoolT | ArrayT ILType
  deriving (Ord, Eq, Show)

data ILExp =
    EInt Int
  | EBool Bool
  | EString String
  | EVar Name
  | EBinOp BinOp ILExp ILExp

  -- | EAlloc ILType ILExp
  -- | EReadIntCSV ILExp
  -- | EReadDoubleCSV ILExp
  -- | RandomIntVector VarName ILExp
  -- | RandomDoubleVector VarName ILExp

data BinOp = AddI | SubI | MulI | DivI

data Stmt =
    Declare Name ILExp
  | Alloc Name ILType ILExp
  | Distribute Level Name ILExp [Stmt]
  | ParFor Level Name ILExp [Stmt]
  | Synchronize
  | Assign Name ILExp
  | AssignSub Name ILExp ILExp
  | Cond ILExp [Stmt] [Stmt]
  | While ILExp [Stmt]
  | ReadIntCSV Name ILExp
  | PrintIntArray ILExp ILExp
  -- | Benchmark Name [Stmt]

type ILProgram = [Stmt]

liveInExp :: ILExp -> Set Name
liveInExp e =
  case e of
    EVar name           -> Set.singleton name
    -- Recursive
    EBinOp _ e0 e1      -> liveInExp e0 `Set.union` liveInExp e1
    -- Scalars and constants
    EInt _              -> Set.empty
    EBool _             -> Set.empty
    EString _           -> Set.empty

freeVars :: [Stmt] -> Set Name
freeVars stmts =
  let
    freeInExp :: Set Name -> ILExp -> Set Name
    freeInExp bound e = liveInExp e `difference` bound

    
    fv :: Set Name -> [Stmt] -> Set Name
    fv _ [] = Set.empty
    -- binding forms
    fv bound (Declare x e : ss) = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (Alloc x _ e : ss) = freeInExp bound e `union` fv (insert x bound) ss
    fv bound (ReadIntCSV x e : ss) = freeInExp bound e `union` fv (insert x bound) ss
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
    -- other
    fv bound (Synchronize : ss) = fv bound ss
    fv bound (Assign x e : ss) = (insert x (liveInExp e) `difference` bound) `union` fv bound ss
    fv bound (AssignSub x e0 e1 : ss) = ((insert x (liveInExp e0) `union` liveInExp e1) `difference` bound) `union` fv bound ss
    fv bound (Cond e ss0 ss1 : ss) =
      freeInExp bound e
      `union` fv bound ss0
      `union` fv bound ss1
      `union` fv bound ss
    fv bound (PrintIntArray e0 e1 : ss) = freeInExp bound e0 `union` freeInExp bound e1 `union` fv bound ss      
    
  in fv Set.empty stmts

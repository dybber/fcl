module Language.GPUIL.Liveness (liveness, LiveInfo) where

import Control.Monad.State
import Data.Set as Set

import Language.GPUIL.Syntax

-- Collection of live variables
type LiveInfo = Set VarName

-- Arrays accessed in the given expression
liveInExp :: IExp ty -> LiveInfo
liveInExp e =
  case e of
    IndexE name e0      -> insert name (liveInExp e0)
    VarE name@(_, CPtr _ _) _ -> singleton name
    VarE _ _            -> empty
    -- Recursive
    UnaryOpE _ e0       -> liveInExp e0
    BinOpE _ e0 e1      -> liveInExp e0 `union` liveInExp e1
    IfE e0 e1 e2 _      -> liveInExp e0 `union` liveInExp e1 `union` liveInExp e2
    CastE _ e0          -> liveInExp e0
    -- Scalars and constants
    IntE _              -> empty
    DoubleE _           -> empty
    BoolE _             -> empty
    Word8E _            -> empty
    Word32E _           -> empty
    GlobalID            -> empty
    LocalID             -> empty
    GroupID             -> empty
    LocalSize           -> empty
    NumGroups           -> empty
    WarpSize            -> empty

type LM a = State LiveInfo a

startState :: LiveInfo
startState = Set.empty

liveness :: Statements a ty -> Statements LiveInfo ty
liveness ss = evalState (liveness' ss) startState

liveness' :: Statements a ty -> LM (Statements LiveInfo ty)
liveness' = liftM reverse . mapM liveStmt . reverse . Prelude.map fst

liveStmt :: Statement a ty -> LM (Statement LiveInfo ty, LiveInfo)
liveStmt SyncGlobalMem =
  do liveSet <- get
     return (SyncGlobalMem, liveSet)
liveStmt SyncLocalMem =
  do liveSet <- get
     return (SyncLocalMem, liveSet)
liveStmt (Decl name Nothing) =
  do liveSet <- get
     return (Decl name Nothing, liveSet)
liveStmt (Decl name (Just e0)) =
  do modify (union (liveInExp e0))
     liveSet <- get
     return (Decl name (Just e0), liveSet)
liveStmt (Assign name e0) =
  do modify (union (insert name (liveInExp e0)))
     liveSet <- get
     return (Assign name e0, liveSet)
liveStmt (AssignSub name e0 idx) =
  do modify (union (insert name (liveInExp e0 `union` liveInExp idx)))
     liveSet <- get
     return (AssignSub name e0 idx, liveSet)
liveStmt (Allocate name size' ty) =
  do modify (delete name)
     liveSet <- get
     return (Allocate name size' ty, liveSet)
liveStmt (If e0 ss_then ss_else) =
  do s <- get
     let (ss_then', after_then) = runState (liveness' ss_then) s
         (ss_else', after_else) = runState (liveness' ss_else) s
         newLiveSet = after_then `union` after_else
     put newLiveSet
     return (If e0 ss_then' ss_else', newLiveSet)
liveStmt (For name bound ss) =
  do ss' <- liveness' ss
     modify (union (liveInExp bound))
     newLiveSet <- get
     return (For name bound ss', newLiveSet)
liveStmt (SeqWhile bound ss) =
  do ss' <- liveness' ss
     modify (union (liveInExp bound))
     newLiveSet <- get
     return (SeqWhile bound ss', newLiveSet)
liveStmt (ForAll lvl name bound ss) =
  do ss' <- liveness' ss
     modify (union (liveInExp bound))
     newLiveSet <- get
     return (ForAll lvl name bound ss', newLiveSet)
liveStmt (DistrPar lvl name bound ss) =
  do ss' <- liveness' ss
     modify (union (liveInExp bound))
     newLiveSet <- get
     return (DistrPar lvl name bound ss', newLiveSet)

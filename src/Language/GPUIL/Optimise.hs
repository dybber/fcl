module Language.GPUIL.Optimise (optimise, optimiseExp) where

import Language.GPUIL.Syntax
import qualified Data.Map as Map
import Control.Monad.State

optimise :: (Show ty, Eq ty) => Statements a ty -> Statements a ty
optimise = constantFold Map.empty

optimiseExp :: (Show ty, Eq ty) => IExp ty -> IExp ty
optimiseExp = foldExp Map.empty


type Env ty = Map.Map VarName (IExp ty)

foldVar :: (Show ty, Eq ty) => Env ty -> VarName -> VarName
foldVar env x =
  case Map.lookup x env of
    Just (VarE y _)  -> y
    _ -> x

foldExp :: (Show ty, Eq ty) => Env ty -> IExp ty -> IExp ty
foldExp env e =
  case e of
    IntE _     -> e
    DoubleE _  -> e
    BoolE _    -> e
    Word8E _   -> e
    Word32E _  -> e
    GlobalID   -> e
    LocalID    -> e
    GroupID    -> e
    LocalSize  -> e
    NumGroups  -> e
    WarpSize   -> e
    VarE x _  -> e
      -- case Map.lookup x env of
      --   Just v  -> v
      --   Nothing -> e
    CastE _ e0  -> foldExp env e0
    IndexE var e0 -> IndexE (foldVar env var) (foldExp env e0)
    IfE e0 e1 e2 ty -> foldIf (foldExp env e0) (foldExp env e1) (foldExp env e2) ty
    BinOpE op e0 e1 -> foldBinOp op (foldExp env e0) (foldExp env e1)
    UnaryOpE op e0 -> foldUnOp op (foldExp env e0)

foldIf :: (Show ty, Eq ty) => IExp ty -> IExp ty -> IExp ty -> ty -> IExp ty
foldIf (BoolE True) e1 _ _ = e1
foldIf (BoolE False) _ e2 _ = e2
foldIf e0 (BoolE True) e2 _  | e0 == e2 = e0
foldIf e0 (BoolE False) e2 _ | e0 == e2 = BoolE False
foldIf e0 e1 (BoolE False) _ | e0 == e1 = e0
foldIf e0 e1 (BoolE True) _  | e0 == e1 = BoolE True
foldIf e0 e1 e2 ty = IfE e0 e1 e2 ty
-- TODO: nested ifs

foldUnOp :: UnaryOp -> IExp ty -> IExp ty
foldUnOp Not (BoolE True) = BoolE False
foldUnOp Not (BoolE False) = BoolE True
foldUnOp Not (UnaryOpE Not e) = e
foldUnOp op e = UnaryOpE op e
-- TODO

foldBinOp :: BinOp -> IExp ty -> IExp ty -> IExp ty
foldBinOp AddI (IntE v0) (IntE v1) = IntE (v0 + v1)
foldBinOp AddI (IntE 0) e1 = e1
foldBinOp AddI e0 (IntE 0) = e0
foldBinOp AddI (BinOpE SubI e0 (IntE v1)) (IntE v2) =
  foldBinOp AddI e0 (IntE (v2-v1)) -- (a - b) + c ==> a + (c-b)
foldBinOp AddI (BinOpE SubI (IntE v0) e1) (IntE v2) =
  foldBinOp SubI (IntE (v0+v2)) e1 -- (a - b) + c ==> (a + c) -b

foldBinOp SubI (IntE v0) (IntE v1) = IntE (v0 - v1)
foldBinOp SubI (IntE 0) e1 = UnaryOpE NegateInt e1
foldBinOp SubI e0 (IntE 0) = e0

foldBinOp MulI (IntE v0) (IntE v1) = IntE (v0 * v1)
foldBinOp MulI (IntE 1) e1 = e1
foldBinOp MulI e0 (IntE 1) = e0
foldBinOp MulI (IntE 0) _ = IntE 0
foldBinOp MulI _ (IntE 0) = IntE 0

foldBinOp DivI (IntE v0) (IntE v1) = IntE (v0 `div` v1)
foldBinOp DivI (IntE 0) _ = IntE 0
foldBinOp DivI e0 (IntE 1) = e0

foldBinOp ModI (IntE v0) (IntE v1) = IntE (v0 `mod` v1)
foldBinOp ModI _ (IntE 1) = IntE 0

foldBinOp EqI (IntE v0) (IntE v1) | v0 == v1  = BoolE True
                                  | otherwise = BoolE False

foldBinOp op e0 e1 = BinOpE op e0 e1



constantFold :: (Show ty, Eq ty) => Env ty -> Statements a ty -> Statements a ty
constantFold environment stmts = evalState (liftM concat $ mapM process stmts) environment
 where
--   process :: (Statement a ty, ()) -> State (Env ty) (Statement a ty, ())
   process ((For v e body),i) =
     do env <- get
        return [(For v (foldExp env e) (constantFold env body), i)]
   process (DistrPar lvl v e body,i) =
     do env <- get
        return [(DistrPar lvl v e (constantFold env body), i)]
   process (ForAll lvl v e body,i) =
     do env <- get
        return [(ForAll lvl v (foldExp env e) (constantFold env body), i)]
   process (If e strue sfalse,i) =
     do env <- get
        case foldExp env e of
          BoolE True -> return (constantFold env strue)
          BoolE False -> return (constantFold env sfalse)
          e' -> return [(If e' (constantFold env strue)
                               (constantFold env sfalse), i)]
   process (SeqWhile e body,i) =
     do env <- get
        return [(SeqWhile (foldExp env e) (constantFold env body), i)]
   process (Decl v Nothing,i) = return [(Decl v Nothing, i)]
   process (Decl v@(name,CPtr _ _) (Just e),i) =
     do env <- get
        return [(Decl v (Just (foldExp env e)), i)]
   process (Decl v (Just e),i) =
     do env <- get
        let e' = foldExp env e
        if isScalar e' -- || isVar e'
          then do put (Map.insert v e' env)
                  return []
          else return [(Decl v (Just e'), i)]
   process (SyncLocalMem,i) = return [(SyncLocalMem,i)]
   process (SyncGlobalMem,i) = return [(SyncGlobalMem,i)]
   process (Allocate v e ty,i) = do env <- get
                                    return [(Allocate v (foldExp env e) ty,i)]
   process (Assign v e, i) = do env <- get
                                return [(Assign (foldVar env v) (foldExp env e), i)]
   process (AssignSub v e0 e1, i) = do env <- get
                                       return [(AssignSub (foldVar env v) (foldExp env e0) (foldExp env e1), i)]
   process (Comment msg, i) = return [(Comment msg, i)]

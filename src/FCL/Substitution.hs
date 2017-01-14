module FCL.Substitution (apply, freeIn) where

import Control.Monad.Trans.State
import qualified Data.Set as Set

import FCL.Core.SourceRegion
import FCL.Core.Identifier
import FCL.Core.Syntax

------------------
-- Substitution --
------------------
freeIn :: Identifier -> Exp ty -> Bool
freeIn _ (Literal _ _)            = True
freeIn x (UnaryOp _ e _)          = freeIn x e
freeIn x (BinaryOp _ e1 e2 _)     = freeIn x e1 && freeIn x e2
freeIn x (Var y _ _)              = x /= y
freeIn x (Vec es _ _)             = all (freeIn x) es
freeIn x (Lamb y _ e _ _)         = x == y || freeIn x e
freeIn x (App e1 e2)              = freeIn x e1 && freeIn x e2
freeIn x (LambLvl _ e _ _)        = freeIn x e
freeIn x (AppLvl e _)             = freeIn x e
freeIn x (Let y e ebody _ _)      = freeIn x e && (x == y || freeIn x ebody)
freeIn x (Cond e1 e2 e3 _ _)      = all (freeIn x) [e1, e2, e3]
freeIn x (LengthPull e _)         = freeIn x e
freeIn x (LengthPush e _)         = freeIn x e
freeIn x (Push _ e _)             = freeIn x e
freeIn x (Force e _)              = freeIn x e
freeIn x (Proj1E e _)             = freeIn x e
freeIn x (Proj2E e _)             = freeIn x e
freeIn x (Pair e1 e2 _)           = freeIn x e1 && freeIn x e2
freeIn x (Index e1 e2 _)          = freeIn x e1 && freeIn x e2
freeIn x (GeneratePull e1 e2 _)   = freeIn x e1 && freeIn x e2
freeIn x (MapPull e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (MapPush e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (Interleave e1 e2 e3 _)  = all (freeIn x) [e1, e2, e3]
freeIn x (For e1 e2 e3 _)         = all (freeIn x) [e1, e2, e3]
freeIn x (Power e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn x (While e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn x (WhileSeq e1 e2 e3 _)    = all (freeIn x) [e1, e2, e3]
freeIn _ (BlockSize _)            = True
freeIn x (Return _ e _)           = freeIn x e
freeIn x (Bind e1 e2 _)           = freeIn x e1 && freeIn x e2
freeIn x (ForceAndPrint e1 e2 _)  = freeIn x e1 && freeIn x e2
freeIn x (Benchmark e1 e2 _)      = freeIn x e1 && freeIn x e2
freeIn x (ReadIntCSV e1 _)        = freeIn x e1

freeVars :: Exp ty -> Set.Set Identifier
freeVars (Literal _ _)            = Set.empty
freeVars (UnaryOp _ e _)          = freeVars e
freeVars (BinaryOp _ e1 e2 _)     = Set.union (freeVars e1) (freeVars e2)
freeVars (Var x _ _)              = Set.singleton x
freeVars (Vec es _ _)             = Set.unions (map freeVars es)
freeVars (Lamb x _ e _ _)         = Set.difference (freeVars e) (Set.singleton x)
freeVars (App e1 e2)              = Set.union (freeVars e1) (freeVars e2)
freeVars (LambLvl _ e _ _)        = freeVars e
freeVars (AppLvl e _)             = freeVars e
freeVars (Let x e1 e2 _ _)        = Set.union (freeVars e1)
                                              (Set.difference (freeVars e2) (Set.singleton x))
freeVars (Cond e1 e2 e3 _ _)      = Set.unions (map freeVars [e1, e2, e3])
freeVars (Force e _)              = freeVars e
freeVars (Push _ e _)             = freeVars e
freeVars (LengthPull e _)         = freeVars e
freeVars (LengthPush e _)         = freeVars e
freeVars (Proj1E e _)             = freeVars e
freeVars (Proj2E e _)             = freeVars e
freeVars (Pair e1 e2 _)           = Set.union (freeVars e1) (freeVars e2)
freeVars (Index e1 e2 _)          = Set.union (freeVars e1) (freeVars e2)
freeVars (GeneratePull e1 e2 _)   = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPull e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPush e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (Interleave e1 e2 e3 _)  = Set.unions (map freeVars [e1, e2, e3])
freeVars (For e1 e2 e3 _)         = Set.unions (map freeVars [e1, e2, e3])
freeVars (Power e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (While e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (WhileSeq e1 e2 e3 _)    = Set.unions (map freeVars [e1, e2, e3])
freeVars (BlockSize _)            = Set.empty
freeVars (Return _ e _)           = freeVars e
freeVars (Bind e1 e2 _)           = Set.union (freeVars e1) (freeVars e2)
freeVars (ForceAndPrint e1 e2 _)  = Set.union (freeVars e1) (freeVars e2)
freeVars (Benchmark e1 e2 _)      = Set.union (freeVars e1) (freeVars e2)
freeVars (ReadIntCSV e1 _)        = freeVars e1

freshVar :: State [Identifier] Identifier
freshVar =
  do (x:xs) <- get
     put xs
     return x

apply :: (Identifier, Exp ty) -> Exp ty -> Exp ty
apply (x, ebody) e =
  let vars = [Identifier ('x' : show (i :: Int)) | i <- [0..]]
  in evalState (subst e x ebody) vars

subst :: Exp ty -> Identifier -> Exp ty -> State [Identifier] (Exp ty)
subst s x e =
  case e of
   -- Base cases
    Literal _ _ -> return e
    (Var y _ _)
      | x == y    -> return s
      | otherwise -> return e

   -- Interesting cases
    (App e1 e2)   ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (App e1' e2')
    (Lamb y ty1 ebody ty2 r)
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             ebody' <- subst (Var z ty1 Missing) y ebody
             ebody'' <- subst s x ebody'
             return (Lamb z ty1 ebody'' ty2 r)
      | otherwise ->
          do ebody' <- subst s x ebody
             return (Lamb y ty1 ebody' ty2 r)
    (AppLvl e0 lvl)   ->
          do e0' <- subst s x e0
             return (AppLvl e0' lvl)
    (LambLvl lvlvar ebody ty r) ->
           do ebody' <- subst s x ebody
              return (LambLvl lvlvar ebody' ty r)
    (Let y e1 e2 t r)
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             e1' <- subst s x e1
             e2' <- subst (Var z t Missing) y e2 -- TODO the "t" here is REALLY wrong
             e2'' <- subst s x e2'
             return (Let z e1' e2'' t r)
      | otherwise ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (Let y e1' e2' t r)

   -- Recurse in all other cases
    UnaryOp op e1 r ->
       do e1' <- subst s x e1
          return (UnaryOp op e1' r)
    BinaryOp op e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (BinaryOp op e1' e2' r)
    Vec es ty r ->
      do es' <- mapM (subst s x) es
         return (Vec es' ty r)
    Cond e1 e2 e3 ty r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Cond e1' e2' e3' ty r)
    Pair e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Pair e1' e2' r)
    Proj1E e1 r ->
       do e1' <- subst s x e1
          return (Proj1E e1' r)
    Proj2E e1 r ->
       do e1' <- subst s x e1
          return (Proj2E e1' r)
    Index e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Index e1' e2' r)
    LengthPull e1 r ->
       do e1' <- subst s x e1
          return (LengthPull e1' r)
    LengthPush e1 r ->
       do e1' <- subst s x e1
          return (LengthPush e1' r)
    For e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (For e1' e2' e3' r)
    Power e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Power e1' e2' e3' r)
    While e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (While e1' e2' e3' r)
    WhileSeq e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (WhileSeq e1' e2' e3' r)
    GeneratePull e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (GeneratePull e1' e2' r)
    MapPull e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (MapPull e1' e2' r)
    MapPush e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (MapPush e1' e2' r)
    Force e1 r ->
       do e1' <- subst s x e1
          return (Force e1' r)
    Push lvl e1 r ->
       do e1' <- subst s x e1
          return (Push lvl e1' r)
    Interleave e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Interleave e1' e2' e3' r)
    BlockSize _ -> return e
    Return lvl e1 r ->
      do e1' <- subst s x e1
         return (Return lvl e1' r)
    Bind e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (Bind e1' e2' r)
    ForceAndPrint e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (ForceAndPrint e1' e2' r)
    Benchmark e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (Benchmark e1' e2' r)
    ReadIntCSV e1 r ->
      do e1' <- subst s x e1
         return (ReadIntCSV e1' r)


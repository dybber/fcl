-- | After parsing all type variables will be numbered 0
-- 
-- This just iterates over the complete AST assigning numbers to type
-- and level variables in State-monad
module Language.FCL.InitTyVars (initTyVars) where

import qualified Data.Map as Map
import Control.Monad.Trans.State

import Language.FCL.Syntax

initTyVars :: Show ty => Program ty -> (Program ty, Int)
initTyVars prog = runState (mapM initDefinition prog) 0

initDefinition :: Show ty => Definition ty -> State Int (Definition ty)
initDefinition d =
  do newBody <- initLvlVars_exp Map.empty (defBody d)
     case defSignature d of
       Just sig ->
         do (sig', _) <- initTyVars_typ Map.empty sig
            (sig'', _) <- initLvlVars_typ Map.empty sig'
            return (d { defSignature =  Just sig''
                      , defBody = newBody })
       Nothing -> return (d { defBody = newBody })
  
newtv :: State Int Type
newtv = do
 i <- get
 put (i+1)
 return (VarT (TyVar i Nothing))

newNamedTV :: String -> State Int Type
newNamedTV name = do
 i <- get
 put (i+1)
 return (VarT (TyVar i (Just name)))

newLvlVar :: State Int LvlVar
newLvlVar = do
 i <- get
 put (i+1)
 return (LvlVar i Nothing)

newNamedLvlVar :: String -> State Int LvlVar
newNamedLvlVar name = do
 i <- get
 put (i+1)
 return (LvlVar i (Just name))

-- Initialize all type variables in the signature, by associating
-- a number with each
initTyVars_typ :: Map.Map Name Type -> Type -> State Int (Type, Map.Map Name Type)
-- base cases
initTyVars_typ env IntT    = return (IntT,env)
initTyVars_typ env DoubleT = return (DoubleT,env)
initTyVars_typ env BoolT   = return (BoolT,env)
-- Interesting cases
initTyVars_typ env (VarT (TyVar _ Nothing)) =
  do tv <- newtv
     return (tv, env)
initTyVars_typ env (VarT (TyVar _ (Just x))) =
  case Map.lookup x env of
    Just tv -> return (tv, env)
    Nothing ->
      do tv <- newNamedTV x
         return (tv, Map.insert x tv env)
-- Recurse
initTyVars_typ env (t0 :> t1) =
  do (t0',env')  <- initTyVars_typ env t0
     (t1',env'') <- initTyVars_typ env' t1
     return (t0' :> t1', env'')
initTyVars_typ env (lvl :-> t) =
  do (t',env') <- initTyVars_typ env t
     return (lvl :-> t', env')
initTyVars_typ env (t0 :*: t1) =
  do (t0',env')  <- initTyVars_typ env t0
     (t1',env'') <- initTyVars_typ env' t1
     return (t0' :*: t1', env'')  
initTyVars_typ env (PullArrayT t) =
  do (t', env') <- initTyVars_typ env t
     return (PullArrayT t', env')
initTyVars_typ env (PushArrayT lvl t) =
  do (t', env') <- initTyVars_typ env t
     return (PushArrayT lvl t', env')

initLvlVars_typ :: Map.Map Name LvlVar -> Type -> State Int (Type, Map.Map Name LvlVar)
initLvlVars_typ env ((LvlVar _ Nothing) :-> t) =
  do lvlvar <- newLvlVar
     return (lvlvar :-> t, env)
initLvlVars_typ env ((LvlVar _ (Just name)) :-> t) =
  case Map.lookup name env of
    Just v -> error ("LvlVar " ++ show v ++ " already bound")
    Nothing ->
      do lvlvar <- newNamedLvlVar name
         return (lvlvar :-> t, Map.insert name lvlvar env)
initLvlVars_typ env (PushArrayT lvl t) =
  do lvl' <- initLvlVars_lvl env lvl
     (t', env'') <- initLvlVars_typ env t
     return (PushArrayT lvl' t', env'')
initLvlVars_typ env IntT = return (IntT, env)
initLvlVars_typ env DoubleT = return (DoubleT, env)
initLvlVars_typ env BoolT = return (BoolT, env)
initLvlVars_typ env v@(VarT _) = return (v, env)
initLvlVars_typ env (t0 :> t1) =
  do (t0',env')  <- initLvlVars_typ env t0
     (t1',env'') <- initLvlVars_typ env' t1
     return (t0' :> t1', env'')
initLvlVars_typ env (t0 :*: t1) =
  do (t0',env')  <- initLvlVars_typ env t0
     (t1',env'') <- initLvlVars_typ env' t1
     return (t0' :*: t1', env'')  
initLvlVars_typ env (PullArrayT t) =
  do (t', env') <- initLvlVars_typ env t
     return (PullArrayT t', env')

initLvlVars_lvl :: Map.Map Name LvlVar -> Level -> State Int Level
initLvlVars_lvl _ Zero = return (Zero)
initLvlVars_lvl env (Step lvl) =
  do lvl' <- initLvlVars_lvl env lvl
     return (Step lvl')
initLvlVars_lvl _ (VarL (LvlVar _ Nothing)) =
  do lvlvar <- newLvlVar
     return (VarL lvlvar)
initLvlVars_lvl env (VarL (LvlVar _ (Just name))) =
  case Map.lookup name env of
    Just lvl -> return (VarL lvl)
    Nothing ->
      do lvlvar <- newNamedLvlVar name
         return (VarL lvlvar)

initLvlVars_exp :: Show ty => Map.Map Name LvlVar -> Exp ty -> State Int (Exp ty)
initLvlVars_exp _ e@(IntScalar _ _) = return e
initLvlVars_exp _ e@(DoubleScalar _ _) = return e
initLvlVars_exp _ e@(BoolScalar _ _) = return e
initLvlVars_exp _ e@(Var _ _ _) = return e
initLvlVars_exp env (UnOp op e r) =
  do e' <- initLvlVars_exp env e
     return (UnOp op e' r)
initLvlVars_exp env (BinOp op e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (BinOp op e1' e2' r)
initLvlVars_exp env (Lamb x ty1 e ty2 r) =
  do e' <- initLvlVars_exp env e
     return (Lamb x ty1 e' ty2 r)
initLvlVars_exp env (Let x e1 e2 ty r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (Let x e1' e2' ty r)
initLvlVars_exp env (App e1 e2) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (App e1' e2')
initLvlVars_exp env (LambLvl (LvlVar _ Nothing) e ty r) =
  do lvlvar <- newLvlVar
     e' <- initLvlVars_exp env e
     return (LambLvl lvlvar e' ty r)
initLvlVars_exp env (LambLvl (LvlVar _ (Just name)) e ty r) =
  do lvlvar <- newNamedLvlVar name
     let env' = Map.insert name lvlvar env
     e' <- initLvlVars_exp env' e
     return (LambLvl lvlvar e' ty r)
initLvlVars_exp env (AppLvl e lvl) =
  do e' <- initLvlVars_exp env e
     lvl' <- initLvlVars_lvl env lvl
     return (AppLvl e' lvl')
initLvlVars_exp env (Cond e1 e2 e3 ty r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     e3' <- initLvlVars_exp env e3
     return (Cond e1' e2' e3' ty r)  
initLvlVars_exp env (Pair e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (Pair e1' e2' r)  
initLvlVars_exp env (Proj1E e1 r) =
  do e1' <- initLvlVars_exp env e1
     return (Proj1E e1' r)  
initLvlVars_exp env (Proj2E e1 r) =
  do e1' <- initLvlVars_exp env e1
     return (Proj2E e1' r)  
initLvlVars_exp env (Index e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (Index e1' e2' r)  
initLvlVars_exp env (LengthPull e1 r) =
  do e1' <- initLvlVars_exp env e1
     return (LengthPull e1' r)  
initLvlVars_exp env (LengthPush e1 r) =
  do e1' <- initLvlVars_exp env e1
     return (LengthPush e1' r)
initLvlVars_exp env (While e1 e2 e3 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     (e3') <- initLvlVars_exp env e3
     return (While e1' e2' e3' r)  
initLvlVars_exp env (WhileSeq e1 e2 e3 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     (e3') <- initLvlVars_exp env e3
     return (WhileSeq e1' e2' e3' r)  
initLvlVars_exp env (GeneratePull e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (GeneratePull e1' e2' r)  
initLvlVars_exp env (MapPull e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (MapPull e1' e2' r)  
initLvlVars_exp env (MapPush e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (MapPush e1' e2' r)  
initLvlVars_exp env (Force e1 r) =
  do e1' <- initLvlVars_exp env e1
     return (Force e1' r)  
initLvlVars_exp env (Push lvl e r) =
  do lvl' <- initLvlVars_lvl env lvl
     e'  <- initLvlVars_exp env e
     return (Push lvl' e' r)
initLvlVars_exp env (Concat e1 e2 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     return (Concat e1' e2' r)  
initLvlVars_exp env (Interleave e1 e2 e3 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     (e3') <- initLvlVars_exp env e3
     return (Interleave e1' e2' e3' r)  
initLvlVars_exp _ (BlockSize r) = return (BlockSize r)  
initLvlVars_exp env (Scanl e1 e2 e3 r) =
  do e1' <- initLvlVars_exp env e1
     e2' <- initLvlVars_exp env e2
     (e3') <- initLvlVars_exp env e3
     return (Scanl e1' e2' e3' r)  
initLvlVars_exp _ (Vec _ _ _) = error "initLvlVars_exp: Vec not implemented"

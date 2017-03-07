module FCL.Infer.Unification
  (unify, unifyAll,
   checkAnnotation,
   unifyLvlVar, occurs)
where

import qualified Data.Map as Map
import Control.Monad.Trans.State

import FCL.Core.Polytyped
import FCL.Core.PolyLevel
import FCL.Substitution
import FCL.Infer.Monad

unify :: Type -> Type -> TI ()
unify t1 t2 =
  do t1' <- tvchase t1
     t2' <- tvchase t2
     unify' t1' t2'

unify' :: Type -> Type -> TI ()
unify' t1 t2 =
  case (t1, t2) of
    (IntT, IntT) -> return ()
    (BoolT, BoolT) -> return ()
    (DoubleT, DoubleT) -> return ()
    (StringT, StringT) -> return ()
    (t1a :> t1r, t2a :> t2r) ->
        do unify t1r t2r
           unify t1a t2a
    (t1a :*: t1r, t2a :*: t2r) ->
        do unify t1r t2r
           unify t1a t2a
    (PullArrayT t1', PullArrayT t2') -> unify t1' t2'
    (PushArrayT lvl1 t1', PushArrayT lvl2 t2') ->
      do unifyLevels lvl1 lvl2
         unify t1' t2'
    (ProgramT lvl1 t1', ProgramT lvl2 t2') ->
      do unifyLevels lvl1 lvl2
         unify t1' t2'
    (VarT v1, t2') -> unify_fv v1 t2'
    (t1', VarT v2) -> unify_fv v2 t1'
    (t1', t2') -> throwError (UnificationError t1' t2')

-- TODO check that the right variables are quantified
checkAnnotation :: TypeScheme -> TypeScheme -> TI ()
checkAnnotation tysc@(TypeScheme _ _ ty)
                tysc_anno@(TypeScheme _ _ tyanno) =
  if evalState (checkAnno ty tyanno) (Map.empty, Map.empty)
    then return ()
    else throwError (SignatureMismatch tysc tysc_anno)


type Check a = State (Map.Map TyVar TyVar, Map.Map LvlVar LvlVar) a

checkAnno :: Type -> Type -> Check Bool
checkAnno ty1 ty2 =
  case (ty1, ty2) of
    (IntT, IntT)       -> return True
    (DoubleT, DoubleT) -> return True
    (BoolT, BoolT)     -> return True
    (StringT, StringT) -> return True
    (VarT tv1, VarT tv2) ->
      do (env, envlvls) <- get
         case Map.lookup tv1 env of
           Just tv2' ->
             if tv2' == tv2
               then return True
               else return False
           Nothing ->
             if tv2 `elem` (Map.elems env) -- TODO bad, please optimize
               then return False
               else do put (Map.insert tv1 tv2 env, envlvls)
                       return True
    (UnitT, UnitT) -> return True
    (t1a :> t1r, t2a :> t2r) ->
      do ca <- checkAnno t1a t2a
         cr <- checkAnno t1r t2r
         return (ca && cr)
    (t1a :*: t1r, t2a :*: t2r) ->
      do ca <- checkAnno t1a t2a
         cr <- checkAnno t1r t2r
         return (ca && cr)
    (PullArrayT t1, PullArrayT t2) -> checkAnno t1 t2
    (PushArrayT lvl1 t1, PushArrayT lvl2 t2) ->
      do cl <- checkLevels lvl1 lvl2
         c <- checkAnno t1 t2
         return (cl && c)
    (ProgramT lvl1 t1, ProgramT lvl2 t2) ->
      do cl <- checkLevels lvl1 lvl2
         c <- checkAnno t1 t2
         return (cl && c)
    (_,_) -> return False

checkLevels :: Level -> Level -> Check Bool
checkLevels Zero Zero = return True
checkLevels (Step lvl1) (Step lvl2) = checkLevels lvl1 lvl2
checkLevels (VarL lv1) (VarL lv2) =
  do (env, envlvls) <- get
     case Map.lookup lv1 envlvls of
       Just lv2' ->
         if lv2' == lv2
           then return True
           else return False
       Nothing ->
         if lv2 `elem` (Map.elems envlvls) -- TODO bad, please optimize
           then return False
           else do put (env, Map.insert lv1 lv2 envlvls)
                   return True
checkLevels _ _ = return False

unify_fv :: TyVar -> Type -> TI ()
unify_fv tv t@(VarT tv') | tv == tv'   = return ()
                         | otherwise   = tvext (tv, t)
unify_fv tv t =
  do (TVE _ s) <- get
     let c = occurs s tv t 
     if c
       then throwError (OccursCheckFailed tv t)
       else tvext (tv, t)

unifyAll :: [Type] -> TI Type
unifyAll [] = VarT `fmap` newtv
unifyAll [t] = return t
unifyAll (t1 : t2 : ts) =
  do unify t1 t2
     unifyAll (t2 : ts)

unifyLevels :: Level -> Level -> TI ()
unifyLevels l0 l1 =
  do l0' <- lvlVarChase l0
     l1' <- lvlVarChase l1
     unifyLvls' l0' l1'

unifyLvls' :: Level -> Level -> TI ()
unifyLvls' l0 l1 =
  case (l0, l1) of
    (Zero, Zero)         -> return ()
    (Step l0', Step l1') -> unifyLvls' l0' l1'
    (VarL lvlVar, lvl)   -> unifyLvlVar lvlVar lvl
    (lvl, VarL lvlVar)   -> unifyLvlVar lvlVar lvl
    (_, _)               -> throwError (LevelUnificationError l0 l1)

unifyLvlVar :: LvlVar -> Level -> TI ()
unifyLvlVar lvlVar1 lvl@(VarL lvlVar2) | lvlVar1 == lvlVar2  = return ()
                                       | otherwise           = lvlVarExt (lvlVar1, lvl)
unifyLvlVar lvlVar1 lvl =
  do (TVE _ s) <- get
     let c = occursLvl s lvlVar1 lvl
     if c
       then throwError (LevelOccursCheckFailed lvlVar1 lvl)
       else lvlVarExt (lvlVar1, lvl)

occurs :: Subst -> TyVar -> Type -> Bool
occurs _ _ IntT    = False
occurs _ _ BoolT   = False
occurs _ _ DoubleT = False
occurs _ _ StringT = False
occurs _ _ UnitT = False
occurs s tv (t1 :> t2)       = occurs s tv t1 || occurs s tv t2
occurs s tv (t1 :*: t2)      = occurs s tv t1 || occurs s tv t2
occurs s tv (PullArrayT t)   = occurs s tv t
occurs s tv (PushArrayT _ t) = occurs s tv t
occurs s tv (ProgramT _ t)   = occurs s tv t
occurs s tv (VarT tv2) =
  case lookupTy tv2 s of
    Just t  -> occurs s tv t
    Nothing -> tv == tv2

occursLvl :: Subst -> LvlVar -> Level -> Bool
occursLvl s lvlVar lvl =
  case lvl of
    Zero -> False
    (Step lvl') -> occursLvl s lvlVar lvl'
    (VarL lvlVar2) ->
      case lookupLvl lvlVar2 s of
        Just t  -> occursLvl s lvlVar t
        Nothing -> lvlVar == lvlVar2

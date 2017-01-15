module FCL.Infer.Unification (unify, unifyAll, unifyBinOp, unifyUnOp, unifyLvlVar, occurs) where

import qualified Data.Map as Map
import Control.Monad.Trans.State

import FCL.Core.SourceRegion
import FCL.External.Syntax
import FCL.Infer.Substitution
import FCL.Infer.Monad

-- | The unification. If unification failed, return the reason
unify :: SourceRegion -> Type -> Type -> TI ()
unify reg t1 t2 =
  do t1' <- tvchase t1
     t2' <- tvchase t2
     unify' reg t1' t2'

-- | If either t1 or t2 are type variables, they are definitely unbound
unify' :: SourceRegion -> Type -> Type -> TI ()
unify' reg t1 t2 =
  case (t1, t2) of
    (IntT, IntT) -> return ()
    (BoolT, BoolT) -> return ()
    (DoubleT, DoubleT) -> return ()
    (StringT, StringT) -> return ()
    (UnitT, UnitT) -> return ()
    (t1a :> t1r, t2a :> t2r) ->
        do unify reg t1r t2r
           unify reg t1a t2a
    (lvl1 :-> t1r, lvl2 :-> t2r) ->
        do unify reg t1r t2r
           unifyLvls (VarL lvl1) (VarL lvl2)
    (t1l :*: t1r, t2l :*: t2r) ->
        do unify reg t1l t2l
           unify reg t1r t2r
    (PullArrayT t1', PullArrayT t2') -> unify reg t1' t2'
    (PushArrayT lvl1 t1', PushArrayT lvl2 t2') ->
      do unifyLvls lvl1 lvl2
         unify reg t1' t2'
    (ProgramT lvl1 t1', ProgramT lvl2 t2') ->
      do unifyLvls lvl1 lvl2
         unify reg t1' t2'
    (VarT v1, t2') -> unify_fv v1 t2'
    (t1', VarT v2) -> unify_fv v2 t1'
    (t1', t2') -> throwError (UnificationError reg t1' t2')

unify_fv :: TyVar -> Type -> TI ()
unify_fv tv t@(VarT tv') | tv == tv'   = return ()
                         | otherwise   = tvext (tv, t)
unify_fv tv t =
  do (TVE _ s) <- get
     let c = occurs s tv t 
     if c
       then throwError (OccursCheckFailed tv t)
       else tvext (tv, t)

unifyAll :: SourceRegion -> [Type] -> TI Type
unifyAll _ [] = VarT <$> newtv
unifyAll _ [t] = return t
unifyAll r (t1 : t2 : ts) =
  do unify r t1 t2
     unifyAll r (t2 : ts)

unify1 :: SourceRegion -> Type -> Type -> Type -> TI Type
unify1 r t1' tret t1 =
  do unify r t1 t1'
     return tret

unifyUnOp :: UnaryOperator -> SourceRegion -> Type -> TI Type
unifyUnOp AbsI r    = unify1 r IntT IntT
unifyUnOp SignI r   = unify1 r IntT IntT
unifyUnOp NegateI r = unify1 r IntT IntT
unifyUnOp Not r     = unify1 r BoolT BoolT
unifyUnOp B2I r     = unify1 r BoolT IntT
unifyUnOp CLZ r     = unify1 r IntT IntT

unify2 :: SourceRegion -> Type -> Type -> Type -> Type -> Type -> TI Type
unify2 r t1' t2' tret t1 t2 =
  do unify r t1 t1'
     unify r t2 t2'
     return tret

unifyBinOp :: BinaryOperator -> SourceRegion -> Type -> Type -> TI Type
unifyBinOp AddI r    = unify2 r IntT IntT IntT
unifyBinOp SubI r    = unify2 r IntT IntT IntT
unifyBinOp MulI r    = unify2 r IntT IntT IntT
unifyBinOp DivI r    = unify2 r IntT IntT IntT
unifyBinOp ModI r    = unify2 r IntT IntT IntT
unifyBinOp EqI r     = unify2 r IntT IntT BoolT
unifyBinOp NeqI r    = unify2 r IntT IntT BoolT
unifyBinOp PowI r    = unify2 r IntT IntT IntT
unifyBinOp ShiftLI r = unify2 r IntT IntT IntT
unifyBinOp ShiftRI r = unify2 r IntT IntT IntT
unifyBinOp AndI r    = unify2 r IntT IntT IntT
unifyBinOp OrI r     = unify2 r IntT IntT IntT
unifyBinOp XorI r    = unify2 r IntT IntT IntT
unifyBinOp AddR r    = unify2 r DoubleT DoubleT DoubleT
unifyBinOp DivR r    = unify2 r DoubleT DoubleT DoubleT
unifyBinOp PowR r    = unify2 r DoubleT DoubleT DoubleT

unifyLvls :: Level -> Level -> TI ()
unifyLvls Zero Zero           = return ()
unifyLvls (Step l0) (Step l1) = unifyLvls l0 l1
unifyLvls (VarL lvlVar) lvl   = unifyLvlVar lvlVar lvl
unifyLvls lvl (VarL lvlVar)   = unifyLvlVar lvlVar lvl
unifyLvls lvl1 lvl2           = throwError (LevelUnificationError lvl1 lvl2)

unifyLvlVar :: LvlVar -> Level -> TI ()
unifyLvlVar lvlVar1 lvl@(VarL lvlVar2) | lvlVar1 == lvlVar2  = return ()
                                       | otherwise           = lvlVarExt (lvlVar1, lvl)
unifyLvlVar lvlVar1 lvl =
  do (TVE _ s) <- get
     let c = occursLvl s lvlVar1 lvl
     if c
       then throwError OccursCheckFailedLevel
       else lvlVarExt (lvlVar1, lvl)

occurs :: Subst -> TyVar -> Type -> Bool
occurs _ _ IntT    = False
occurs _ _ BoolT   = False
occurs _ _ DoubleT = False
occurs _ _ StringT = False
occurs _ _ UnitT   = False
occurs s tv (t1 :> t2)       = occurs s tv t1 || occurs s tv t2
occurs s tv (_ :-> t)        = occurs s tv t
occurs s tv (t1 :*: t2)      = occurs s tv t1 || occurs s tv t2
occurs s tv (PullArrayT t)   = occurs s tv t
occurs s tv (PushArrayT _ t) = occurs s tv t
occurs s tv (ProgramT _ t)   = occurs s tv t
occurs (stv, slvl) tv (VarT tv2) =
  case Map.lookup tv2 stv of
    Just t  -> occurs (stv, slvl) tv t
    Nothing -> tv == tv2

occursLvl :: Subst -> LvlVar -> Level -> Bool
occursLvl _ _ Zero = False
occursLvl s lvlVar (Step lvl) = occursLvl s lvlVar lvl
occursLvl (stv, slvl) lvlVar (VarL lvlVar2) =
  case Map.lookup lvlVar2 slvl of
    Just t  -> occursLvl (stv, slvl) lvlVar t
    Nothing -> lvlVar == lvlVar2

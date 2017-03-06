module FCL.Infer
 (typeinfer,
  initialTypeEnvironment, 
  TypeEnvironment(..),
  TypeError(..))
where

import qualified Data.Set as Set
import Control.Monad.Trans.State (get)
import Control.Monad (when)

import FCL.Core.Identifier
import FCL.Core.PolyLevel
import qualified FCL.Core.Untyped as Untyped
import qualified FCL.Core.Polytyped as Poly

import FCL.Substitution
import FCL.Infer.Monad
import FCL.Infer.Unification
import FCL.Infer.TypeEnvironment

infer :: TypeEnvironment -> Untyped.Exp -> TI Poly.Exp
infer _ (Untyped.Literal l) = return (Poly.Literal l (Poly.literalType l))
infer _ Untyped.Unit = return (Poly.Unit Poly.UnitT)
infer env (Untyped.App e1 e2) =
  do e1' <- infer env e1
     e2' <- infer env e2
     let t1 = Poly.typeOf e1'
     let t2 = Poly.typeOf e2'
     tv <- Poly.VarT <$> newtv
     unify t1 (t2 Poly.:> tv)
     return (Poly.App e1' e2' tv)
infer env (Untyped.Pair e1 e2) =
  do e1' <- infer env e1
     e2' <- infer env e2
     let t1 = Poly.typeOf e1'
     let t2 = Poly.typeOf e2'
     let t = t1 Poly.:*: t2
     return (Poly.Pair e1' e2' t)
infer env (Untyped.Cond e1 e2 e3) =
  do e1' <- infer env e1
     e2' <- infer env e2
     e3' <- infer env e3
     let t1 = Poly.typeOf e1'
     let t2 = Poly.typeOf e2'
     let t3 = Poly.typeOf e3'
     unify t1 Poly.BoolT
     unify t2 t3
     return (Poly.Cond e1' e2' e3' t2)
infer env (Untyped.Lamb x e) =
  do tv <- Poly.VarT <$> newtv
     e' <- infer (ext env (x, Poly.TypeScheme [] [] tv)) e
     let ty = tv Poly.:> (Poly.typeOf e')
     return (Poly.Lamb x e' ty)
infer env (Untyped.Let x anno lvls e1 e2) =
  do e1' <- infer env e1
     -- create function type
     tysc <- generalize env lvls (Poly.typeOf e1')
     case anno of
       Just anno_tysc -> checkAnnotation tysc anno_tysc
       Nothing -> return ()
     e2' <- infer (ext env (x, tysc)) e2
     return (Poly.Let x tysc lvls e1' e2' (Poly.typeOf e2'))
infer env (Untyped.Symbol x lvls) =
  do tysc <- typeEnvLookup env x
     ty' <- instantiate x tysc lvls
     tv <- Poly.VarT <$> newtv
     unify ty' tv
     return (Poly.Symbol x lvls tv)
     
instantiate :: Identifier -> Poly.TypeScheme -> [Level] -> TI Poly.Type
instantiate x (Poly.TypeScheme tyvars lvlvars t) lvls =
  let mkFreshvars :: [Poly.TyVar] -> TI Subst
      mkFreshvars [] = return emptySubst
      mkFreshvars (tv:tvs) =
        do s <- mkFreshvars tvs
           fresh <- Poly.VarT <$> newtv
           return (insertTy s (tv, fresh))
  in do when (length lvlvars /= length lvls)
             (throwError (NotFullyLevelApplied x))
        s <- mkFreshvars tyvars
        -- add substitutions from lvlvars to concrete lvls
        let slvl = foldl insertLvl s (zip lvlvars lvls)
        return (apply slvl t)

generalize :: TypeEnvironment -> [LvlVar] -> Poly.Type -> TI Poly.TypeScheme
generalize env lvls t =
  -- TODO: check that levels are not free in env, to make sure we avoid name capture
  do TVE _ s1 <- get
     let t' = apply s1 t
     let env' = apply s1 env
     let qs = Set.toList (ftv t' `Set.difference` ftv env')
     return (Poly.TypeScheme qs lvls t')

typeinfer :: TypeEnvironment -> Untyped.Exp -> Either TypeError (Poly.TypeScheme, Poly.Exp)
typeinfer env e =
  do let action = do e' <- infer env e
                     tysc <- generalize env [] (Poly.typeOf e')
                     return (tysc, e')
     ((tysc, etyped), TVE _ s) <- runTI action initState
     return (tysc, apply s etyped)

-- type inference without generalizing outermost
-- typeinferNG :: TypeEnvironment -> Untyped.Exp -> Either TypeError (Poly.Exp)
-- typeinferNG env e =
--   do 
--      let action = infer env e
--      (etyped, TVE _ s) <- runTI action initState
--      return (apply s etyped)


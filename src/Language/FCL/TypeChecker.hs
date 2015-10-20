{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.FCL.TypeChecker where

import Language.FCL.Syntax
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Control.Monad.State
import Control.Applicative
import Data.List (nub)

type SizeEnv = Set.Set SizeVar
type TypeEnv = Map.Map VarName Type

emptySizeEnv = Set.empty
emptyTypeEnv = Map.empty

-- These checks that there are no free size variables
wellformedSize :: SizeEnv -> Size -> Bool
wellformedSize env (SizeVar x)      = Set.member x env
wellformedSize _ (Size i)           = True
wellformedSize env (SizeProd s0 s1) =  wellformedSize env s0 && wellformedSize env s1
wellformedSize env (SizePow s0 s1)  = wellformedSize env s0 && wellformedSize env s1

-- TODO: Update to also check if TyVars are in environment
wellformedType :: SizeEnv -> Type -> Bool
wellformedType _ IntT = True
wellformedType _ DoubleT = True
wellformedType _ BoolT = True
wellformedType _ UnitT = True
wellformedType env (t1 :*: t2) = wellformedType env t1 && wellformedType env t2
wellformedType env (t1 :> t2) = wellformedType env t1 && wellformedType env t2
wellformedType env (s :=> t) = wellformedType (Set.insert s env) t
wellformedType env (ArrayT t s) = wellformedType env t && wellformedSize env s

check :: SizeEnv -> TypeEnv -> Exp NoType -> (Type, Exp Type)
check senv tenv (IntE v)    = (IntT, IntE v)
check senv tenv (DoubleE v) = (DoubleT, DoubleE v)
check senv tenv (BoolE v)   = (BoolT, BoolE v)
check senv tenv UnitE       = (UnitT, UnitE)
check senv tenv (PairE e1 e2) =
  let (t1, e1') = check senv tenv e1
      (t2, e2') = check senv tenv e2
  in (t1 :*: t2, PairE e1' e2')
check senv tenv (Proj1E e) =
  let (t, e') = check senv tenv e
  in case t of
       (t1 :*: t2) -> (t1, Proj1E e')
       _ -> error ""
check senv tenv (Proj2E e) =
  let (t, e') = check senv tenv e
  in case t of
       (t1 :*: t2) -> (t2, Proj2E e')
       _ -> error ""
check senv tenv (VarE x) =
  case Map.lookup x tenv of
    Just t -> (t, VarE x)
    Nothing -> error $ "Unbound variable " ++ x
check senv tenv (LamE x t e NoType) =
  let (t', e') = check senv (Map.insert x t tenv) e
      typ = t :> t'
  in (typ, LamE x t e' typ)
check senv tenv (LamDE s e NoType) =
  let (t, e') = check (Set.insert s senv) tenv e
      typ = s :=> t
  in (typ, LamDE s e' typ)
check senv tenv (AppE e1 e2 NoType) =
  let (t1, e1') = check senv tenv e1
      (t2, e2') = check senv tenv e2
  in case t1 of
       (t1' :> t1'') | t1' == t2 -> (t1'', AppE e1' e2' t1'')
       (t1' :> t1'') -> error "Applying with wrong type"
       _             -> error $ "Using " ++ show t1 ++ " as a function"
check senv tenv (AppDE e s NoType) =
  if not (wellformedSize senv s)
  then error "Applying size with free size variables"
  else let (t, e') = check senv tenv e
       in case t of
           (n :> t') -> (t', AppDE e' s t')
           _         -> error "some error"
check senv tenv (MapE lvl f e NoType) =
  let (ft, f') = check senv tenv f
      (et, e') = check senv tenv e
  in case (ft, et) of
       (ft1 :> ft2, ArrayT et' s) | ft1 == et' -> (ft2, MapE lvl f' e' ft2)
       (t1' :> t1'', _) -> error "Applying with wrong type"
       _             -> error $ "Using " ++ show ft ++ " as a function"
check senv tenv (ToLocalE f NoType) =
  let (ft, f') = check senv tenv f
  in case ft of
       (_ :> _) -> (ft, f')
       _        -> error $ "ToLocalE should be invoked on a function"
check senv tenv (ToGlobalE f NoType) =
  let (ft, f') = check senv tenv f
  in case ft of
       (_ :> _) -> (ft, f')
       _        -> error $ "ToGlobalE should be invoked on a function"
check senv tenv (ReduceSeqE f e arr NoType) =
  let (ft, f') = check senv tenv f
      (et, e') = check senv tenv e
      (arrt, arr') = check senv tenv arr
  in case (ft, arrt) of
       ((at :*: bt) :> at', ArrayT bt' s) -> if at == at' && at == et && bt == bt'
                                             then (ArrayT at' (Size 1),
                                                   ReduceSeqE f' e' arr' (ArrayT at' (Size 1)))
                                             else error "Types does not match in arguments to reduceSeq"
       _        -> error $ "ReduceSeqE expects a binary function and an array as arguments"
check senv tenv (JoinE arr NoType) =
  let (arrt, arr') = check senv tenv arr
  in case arrt of
       (ArrayT (ArrayT at i) j) -> let typ = (ArrayT at (SizeProd i j))
                                   in (typ, JoinE arr' typ)
       _        -> error $ "JoinE should be invoked on an array of arrays"
-- check senv tenv (SplitE s arr NoType) =
--   if not (wellformedSize senv s)
--   then error "Size given as argument to split contains free size variables"
--   else let (arrt, arr') = check senv tenv arr
--        in case arrt of
--             (ArrayT at () -> let typ = (ArrayT at (SizeProd i j))
--                                         in (typ, JoinE arr' typ)
--             _        -> error $ "JoinE should be invoked on an array of arrays"

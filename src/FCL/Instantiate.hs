module FCL.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import FCL.Core.Polytyped
import FCL.Core.PolyLevel
import FCL.Substitution

-------------------
-- Instantiation --
-------------------
data InstantiateError = TypeMismatch Type Type
                      | LevelMismatch Level Level
                      deriving (Show, Eq)

getLvlSubstitutions :: Level -> Level -> Either InstantiateError Subst
getLvlSubstitutions Zero Zero           = return emptySubst
getLvlSubstitutions (Step pl) (Step ml) = getLvlSubstitutions pl ml
getLvlSubstitutions (VarL pl) mono      = return (insertLvl emptySubst (pl, mono))
getLvlSubstitutions poly mono           = Left (LevelMismatch poly mono)

getSubstitutions :: Type -> Type -> Either InstantiateError Subst
getSubstitutions IntT IntT                       = return emptySubst
getSubstitutions BoolT BoolT                     = return emptySubst
getSubstitutions DoubleT DoubleT                 = return emptySubst
getSubstitutions StringT StringT                 = return emptySubst
getSubstitutions UnitT UnitT                 = return emptySubst
getSubstitutions (VarT v) mono                   = return (singleton v mono)
getSubstitutions (PullArrayT pt) (PullArrayT mt) = getSubstitutions pt mt
getSubstitutions (PushArrayT plvl pt) (PushArrayT mlvl mt) =
  do ts <- getSubstitutions pt mt
     ls <- getLvlSubstitutions plvl mlvl
     return (ts `union` ls)
getSubstitutions (ProgramT plvl pt) (ProgramT mlvl mt) =
  do ts <- getSubstitutions pt mt
     ls <- getLvlSubstitutions plvl mlvl
     return (ts `union` ls)
getSubstitutions (pf :> pp) (mf :> mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `union` ps)
getSubstitutions (pf :*: pp) (mf :*: mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `union` ps)
getSubstitutions poly mono = Left (TypeMismatch poly mono)

instantiate :: Exp -> Type -> Either InstantiateError Exp
instantiate expr instanceType = do
  subst <- getSubstitutions (typeOf expr) instanceType
  return (subst `apply` expr)

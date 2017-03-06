module FCL.Infer.Monad where

import qualified Data.Map as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import FCL.Core.Polytyped
import FCL.Core.PolyLevel
import FCL.Core.Identifier
import FCL.Substitution
import FCL.Infer.TypeEnvironment

data TVE = TVE Int Subst
  deriving Show

type TI x = StateT TVE (Except TypeError) x

data TypeError = UnificationError Type Type
               | UnexpectedPolymorphicVariable Identifier
               | UnboundVariableError Identifier
               | UnboundTypeVariableError Identifier
               | OccursCheckFailed TyVar Type
               | LevelUnificationError Level Level
               | LevelOccursCheckFailed LvlVar Level
               | NotFullyLevelApplied Identifier
               | SignatureMismatch Type Type
 deriving Eq

typeEnvLookup :: TypeEnvironment -> Identifier -> TI TypeScheme
typeEnvLookup (TypeEnvironment env) x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundVariableError x)

throwError :: TypeError -> TI a
throwError err = lift (throwE err)

initState :: TVE
initState = TVE 0 emptySubst

runTI :: TI a -> TVE -> Either TypeError (a, TVE)
runTI m s = runExcept (runStateT m s)

newtv :: TI TyVar
newtv = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (TyVar i Nothing)

-- | `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: Type -> TI Type
tvchase (VarT x) = do
  (TVE _ s) <- get
  case lookupTy x s of
    Just t -> tvchase t
    Nothing -> return (VarT x)
tvchase t = return t

tvext :: (TyVar,Type) -> TI ()
tvext (x,ty) = do
 (TVE i s) <- get
 put (TVE i (insertTy s (x, ty)))

newLvlVar :: TI LvlVar
newLvlVar = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (LvlVar i Nothing)

lvlVarExt :: (LvlVar,Level) -> TI ()
lvlVarExt (lvlVar,lvl) = do
 (TVE i s) <- get
 put (TVE i (insertLvl s (lvlVar, lvl)))

lvlVarChase :: Level -> TI Level
lvlVarChase (VarL lvlvar) = do
  (TVE _ s) <- get
  case lookupLvl lvlvar s of
    Just l -> lvlVarChase l
    Nothing -> return (VarL lvlvar)
lvlVarChase l = return l

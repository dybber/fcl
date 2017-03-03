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
               | NotImplementedError String
               | UnexpectedPolymorphicVariable Identifier
               | UnboundVariableError Identifier
               | UnboundTypeVariableError Identifier
               | OccursCheckFailed TyVar Type
               | LevelUnificationError Level Level
               | LevelOccursCheckFailed LvlVar Level
               | NotFullyLevelApplied Identifier
 deriving Eq

instance Show TypeError where
  show (UnificationError ty0 ty1) =
    concat ["Unification error.\n",
            "Cannot unify types: ",
            show ty0,
            " and ",
            show ty1]
  show (NotImplementedError msg) = "Not implemented: " ++ msg
  show (UnexpectedPolymorphicVariable ident) = "Unexpected polymorphic variable: " ++ show ident
  show (UnboundVariableError ident) = "Variable: " ++ show ident ++ " not defined."
  show (UnboundTypeVariableError ident) = "Type variable: " ++ show ident ++ " not defined."
  show (OccursCheckFailed tyvar ty) = "Occurs check failed. " ++ show (VarT tyvar) ++ " found in " ++ show ty
  show (LevelUnificationError l0 l1) =
    concat ["Unification error.\n",
            "Cannot unify levels: ",
            show l0,
            " and ",
            show l1]
  show (LevelOccursCheckFailed lvlvar l) = "Occurs check failed. " ++ show (VarL lvlvar) ++ " found in " ++ show l
  show (NotFullyLevelApplied ident) = "Function " ++ show ident ++ " is not applied to right number of level parameters."

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

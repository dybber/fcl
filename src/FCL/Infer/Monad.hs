module FCL.Infer.Monad where

import qualified Data.Map as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import FCL.Type.Polymorphic
import FCL.Core.Identifier
import FCL.Core.SourceRegion
import FCL.Infer.Substitution (Subst)

data TVE = TVE Int Subst

type TI x = StateT TVE (Except TypeError) x

data TypeError = UnificationError SourceRegion Type Type
               | LevelUnificationError Level Level
               | NotImplementedError String
               | UnboundVariableError Identifier
               | UnboundLevelVariableError Identifier
               | UnboundTypeVariableError Identifier
               | OccursCheckFailed TyVar Type
               | OccursCheckFailedLevel
 deriving (Eq, Show)

throwError :: TypeError -> TI a
throwError err = lift (throwE err)

initEnv :: TVE
initEnv = TVE 0 (Map.empty, Map.empty)

runTI :: TI a -> TVE -> Either TypeError (a, TVE)
runTI m s = runExcept (runStateT m s)

newtv :: TI TyVar
newtv = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (TyVar i Nothing)

newLvlVar :: TI LvlVar
newLvlVar = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (LvlVar i Nothing)


-- | `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: Type -> TI Type
tvchase (VarT x) = do
  (TVE _ (stv,_)) <- get
  case Map.lookup x stv of
    Just t -> tvchase t
    Nothing -> return (VarT x)
tvchase t = return t

tvext :: (TyVar,Type) -> TI ()
tvext (x,ty) = do
 (TVE i (stv, slvl)) <- get
 put (TVE i (Map.insert x ty stv, slvl))


lvlVarExt :: (LvlVar,Level) -> TI ()
lvlVarExt (lvlVar,lvl) = do
 (TVE i (stv, slvl)) <- get
 put (TVE i (stv, Map.insert lvlVar lvl slvl))

-- find main definition
-- walk through the main definition:
 -- instantiate references to built-ins, by instantiating the type at Var
 -- when polymorphic user-defined functions (e.g. splitUp) are used, make
 -- a concrete instanse
 -- collect instantiations, such that future references can reuse them

-- https://github.com/oden-lang/oden/blob/master/src/Oden/Compiler/Monomorphization.hs

module FCL.Monomorphization where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import FCL.Core.Identifier
import FCL.Infer.Substitution
import FCL.External.Syntax
import qualified FCL.Type.Polymorphic as Poly
import qualified FCL.Type.Monomorphic as Mono

data MonomorphizationError = MonomorphizationError
data MonomorphState = State
type Morph a = StateT MonomorphState (Except MonomorphizationError) a

throwError :: MonomorphizationError -> Morph a
throwError = lift . throwE

toMonomorphicLevel :: Poly.Level -> Morph Mono.Level
toMonomorphicLevel Poly.Zero                         = return Mono.Thread
toMonomorphicLevel (Poly.Step Poly.Zero)             = return Mono.Block
toMonomorphicLevel (Poly.Step (Poly.Step Poly.Zero)) = return Mono.Grid
toMonomorphicLevel _                                 = throwError MonomorphizationError

toMonomorphic :: Poly.Type -> Morph Mono.Type
toMonomorphic Poly.IntT                = return Mono.IntT
toMonomorphic Poly.DoubleT             = return Mono.DoubleT
toMonomorphic Poly.BoolT               = return Mono.BoolT
toMonomorphic Poly.StringT             = return Mono.StringT
toMonomorphic Poly.UnitT               = return Mono.UnitT
toMonomorphic (ty0 Poly.:> ty1)        = (Mono.:>) <$> toMonomorphic ty0 <*> toMonomorphic ty1
toMonomorphic (ty0 Poly.:*: ty1)       = (Mono.:*:) <$> toMonomorphic ty0 <*> toMonomorphic ty1
toMonomorphic (Poly.PullArrayT ty)     = Mono.PullArrayT <$> (toMonomorphic ty)
toMonomorphic (Poly.PushArrayT lvl ty) = Mono.PushArrayT <$> (toMonomorphicLevel lvl) <*> (toMonomorphic ty)
toMonomorphic (Poly.ProgramT lvl ty)   = Mono.ProgramT <$> (toMonomorphicLevel lvl) <*> (toMonomorphic ty)
toMonomorphic (Poly.VarT _)            = throwError MonomorphizationError
toMonomorphic (_ Poly.:-> _)           = throwError MonomorphizationError

monotypeOf :: Exp Poly.Type -> Morph Mono.Type
monotypeOf e = toMonomorphic (typeOf e)

findLevelSubst :: Poly.Level -> Poly.Level -> Subst
findLevelSubst Poly.Zero Poly.Zero = emptySubst
findLevelSubst (Poly.Step lvl0) (Poly.Step lvl1) = findLevelSubst lvl0 lvl1
findLevelSubst (Poly.VarL lvlvar) monolvl = (Map.empty, Map.singleton lvlvar monolvl)

findSubstitution :: Poly.Type -> Poly.Type -> Subst
findSubstitution Poly.IntT Poly.IntT       = emptySubst
findSubstitution Poly.DoubleT Poly.DoubleT = emptySubst
findSubstitution Poly.BoolT Poly.BoolT     = emptySubst
findSubstitution Poly.StringT Poly.StringT = emptySubst
findSubstitution Poly.UnitT Poly.UnitT     = emptySubst
findSubstitution (Poly.VarT tyvar) mono = (Map.singleton tyvar mono, Map.empty)
findSubstitution (ty0 Poly.:> ty1) (mono0 Poly.:> mono1) =
  union (findSubstitution ty0 mono0)
        (findSubstitution ty1 mono1)
findSubstitution (ty0 Poly.:*: ty1) (mono0 Poly.:*: mono1) =
  union (findSubstitution ty0 mono0)
        (findSubstitution ty1 mono1)
findSubstitution (Poly.PullArrayT ty0) (Poly.PullArrayT mono0) =
  (findSubstitution ty0 mono0)
findSubstitution (Poly.PushArrayT lvl0 ty0) (Poly.PushArrayT monolvl mono0) =
  union (findLevelSubst lvl0 monolvl)
        (findSubstitution ty0 mono0)
findSubstitution (Poly.ProgramT lvl0 ty0) (Poly.ProgramT monolvl mono0) =
  union (findLevelSubst lvl0 monolvl)
        (findSubstitution ty0 mono0)

-- instantatiate expression
instantiate :: Exp Poly.Type -> Poly.Type -> Exp Poly.Type
instantiate e instanceType =
  let subst = findSubstitution (typeOf e) instanceType
  in tvsubExp subst e

-- instantiate a polymorphic definition
instantiateDefinition :: Definition Poly.Type -> Poly.Type -> Definition Poly.Type
instantiateDefinition = undefined

type Environment = Map.Map Identifier Poly.Type
                        
monomorph :: Environment -> Exp Poly.Type -> Morph (Exp Mono.Type)
monomorph env e =
  case e of
    Literal l reg -> return (Literal l reg)
    Vec es _ reg ->
      do monoty <- monotypeOf e
         mes <- mapM (monomorph env) es
         return (Vec mes monoty reg)
    App e1 e2 reg ->
      do me1 <- monomorph env e1
         me2 <- monomorph env e2
         return (App me1 me2 reg)
    Cond e1 e2 e3 ty reg ->
      do me1 <- monomorph env e1
         me2 <- monomorph env e2
         me3 <- monomorph env e3
         mty <- monotypeOf e
         return (Cond me1 me2 me3 mty reg)
    Pair e1 e2 reg ->
      do me1 <- monomorph env e1
         me2 <- monomorph env e2
         return (Pair me1 me2 reg)
    UnaryOp op e1 reg ->
      do me1 <- monomorph env e1
         return (UnaryOp op me1 reg)
    BinaryOp op e1 e2 reg ->
      do me1 <- monomorph env e1
         me2 <- monomorph env e2
         return (BinaryOp op me1 me2 reg)
    Lamb ident typaram body tybody reg ->
      do monotyParam <- toMonomorphic typaram
         monotyBody <- toMonomorphic tybody
         mbody <- monomorph (Map.insert ident typaram env) body
         return (Lamb ident monotyParam mbody monotyBody reg)
    Let ident e1 ebody ty reg -> undefined
    Symbol ident ty reg -> undefined
    LambLvl lvlvar e ty reg -> undefined
    AppLvl e lvl reg -> undefined

monomorphProgram :: [Definition Poly.Type] -> Either MonomorphizationError [Definition Mono.Type]
monomorphProgram defs = undefined

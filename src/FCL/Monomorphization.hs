{-# LANGUAGE CPP #-}
module FCL.Monomorphization where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import FCL.Core.Identifier
import qualified FCL.Core.PolyLevel as PolyLevel
import qualified FCL.Core.MonoLevel as MonoLevel
import qualified FCL.Core.Polytyped as Poly
import qualified FCL.Core.Monotyped as Mono
import FCL.Instantiate
import FCL.Infer.TypeEnvironment

data MonomorphError =
     NotInScope Identifier
   | MonomorphInstantiateError InstantiateError
   | UnexpectedPolyType Poly.Type
   | UnexpectedPolyLevel PolyLevel.Level

data Binding =
    Predefined Identifier Poly.TypeScheme
  | FunctionArg Identifier
  | FunBinding Identifier Poly.TypeScheme Poly.Exp

data FunReference = FunReference Identifier Poly.Type [PolyLevel.Level] Identifier
  deriving (Eq, Ord, Show)

type MonomorphEnv = Map.Map Identifier Binding

mkInitEnv :: TypeEnvironment -> MonomorphEnv
mkInitEnv (TypeEnvironment tenv) =
  Map.mapWithKey (\ident tysc -> Predefined ident tysc) tenv

type Monomorph a = RWST MonomorphEnv -- reader env
                        (Set.Set FunReference)
                        () -- state
                        (Except MonomorphError)
                        a

lookupIn :: MonomorphEnv -> Identifier -> Monomorph Binding
lookupIn env ident =
  case Map.lookup ident env of
      Nothing             -> lift (throwE (NotInScope ident))
      Just binding        -> return binding

withIdentifier :: Identifier -> Binding -> Monomorph a -> Monomorph a
withIdentifier ident binding = local (Map.insert ident binding)

isReferenceTo :: Identifier -> FunReference -> Bool
isReferenceTo identifier' (FunReference funident _ _ _) =
  identifier' == funident

monomorph :: MonomorphEnv -> Poly.Exp -> Either MonomorphError Mono.Exp
monomorph environment e =
  do (me, _, _) <- runExcept (runRWST (monomorphExp e) environment ())
     return me

monomorphLevel :: PolyLevel.Level -> Monomorph MonoLevel.Level
monomorphLevel PolyLevel.Zero = return MonoLevel.Zero
monomorphLevel (PolyLevel.Step lvl) = MonoLevel.Step <$> monomorphLevel lvl
monomorphLevel lvl@(PolyLevel.VarL _) = lift (throwE (UnexpectedPolyLevel lvl))

monomorphType :: Poly.Type -> Monomorph Mono.Type
monomorphType Poly.IntT = return Mono.IntT
monomorphType Poly.BoolT = return Mono.BoolT
monomorphType Poly.DoubleT = return Mono.DoubleT
monomorphType Poly.StringT = return Mono.StringT
monomorphType Poly.UnitT = return Mono.UnitT
monomorphType ty@(Poly.VarT _) = lift (throwE (UnexpectedPolyType ty))
monomorphType (ty0 Poly.:> ty1) = (Mono.:>) <$> monomorphType ty0 <*> monomorphType ty1
monomorphType (ty0 Poly.:*: ty1) = (Mono.:*:) <$> monomorphType ty0 <*> monomorphType ty1
monomorphType (Poly.PullArrayT ty) = (Mono.PullArrayT) <$> monomorphType ty
monomorphType (Poly.PushArrayT lvl ty) = (Mono.PushArrayT) <$> monomorphLevel lvl <*> monomorphType ty
monomorphType (Poly.ProgramT lvl ty) = (Mono.ProgramT) <$> monomorphLevel lvl <*> monomorphType ty

monomorphExp :: Poly.Exp -> Monomorph Mono.Exp
monomorphExp (Poly.Literal l ty)       = Mono.Literal l <$> monomorphType ty
monomorphExp (Poly.Unit ty)            = Mono.Unit <$> monomorphType ty
monomorphExp (Poly.Lamb ident e ty)    =
  do me <- withIdentifier ident (FunctionArg ident) (monomorphExp e)
     mt <- monomorphType ty
     return (Mono.Lamb ident me mt)
monomorphExp (Poly.App e1 e2 ty)       = Mono.App <$> monomorphExp e1 <*> monomorphExp e2 <*> monomorphType ty
monomorphExp (Poly.Pair e1 e2 ty)      = Mono.Pair <$> monomorphExp e1 <*> monomorphExp e2 <*> monomorphType ty
monomorphExp (Poly.Cond e1 e2 e3 ty)   = Mono.Cond <$> monomorphExp e1 <*> monomorphExp e2 <*> monomorphExp e3 <*> monomorphType ty
monomorphExp (Poly.Let ident tysc lvlparams e1 e2 _) =
  do (me2, allrefs) <- listen (withIdentifier ident (FunBinding ident tysc e1) (monomorphExp e2))
     let (refs, other) = Set.partition (isReferenceTo ident) allrefs
     tell other
     -- convert to lambda bindings
     -- let e1' = unwrapLambdas e1 params
     -- instantiate with concrete types for each usage point
     insts <- mapM (monomorphReference lvlparams e1) (Set.toList refs)
     return (unwrapAsLets insts me2)
monomorphExp (Poly.Symbol ident lvlargs ty) =
  do -- create monomorphic version of function
     -- we can assume that the type assigned to returnty is monomorphic
     env <- ask
     newIdent <- getMonomorphicIn env ident ty lvlargs

     -- call function
     mty <- monomorphType ty -- monomorphic return type
     return (Mono.Symbol newIdent mty)

createFunType :: [Poly.Type] -> Poly.Type -> Poly.Type
createFunType [] returnty = returnty
createFunType (arg:args) returnty = arg Poly.:> createFunType args returnty
  
getMonomorphicFunBinding :: Identifier -> Poly.Type -> [PolyLevel.Level] -> Poly.Type -> Monomorph Identifier
getMonomorphicFunBinding ident mty lvlargs pty =
  do -- is the original binding already monomorphic? otherwise encode a new name
     let newIdent =
           if Poly.isPolymorphicType pty
           then Poly.encodeDef ident mty
           else ident
     -- ask for this instance to be created
     tell (Set.singleton (FunReference ident mty lvlargs newIdent))
     return newIdent

getMonomorphicIn :: MonomorphEnv -> Identifier -> Poly.Type -> [PolyLevel.Level] -> Monomorph Identifier
getMonomorphicIn env ident mty lvlparams =
  do binding <- lookupIn env ident
     case binding of
       FunBinding _ _ expr -> getMonomorphicFunBinding ident mty lvlparams (Poly.typeOf expr)
       Predefined _ _ -> return ident
       FunctionArg _ -> return ident

monomorphReference :: [PolyLevel.LvlVar] -> Poly.Exp -> FunReference -> Monomorph (Identifier, Mono.Exp)
monomorphReference lvlparams e (FunReference _ fty lvlargs newIdent) =
  do instantiated <- if Poly.isPolymorphicType (Poly.typeOf e)
                     then case instantiate e fty of
                            Left err -> lift (throwE (MonomorphInstantiateError err))
                            Right expr -> return expr
                     else return e
     me <- monomorphExp instantiated
     return (newIdent, me)

-- Convert function application of arbitrary number of arguments to
-- many lambda applications
-- arguments must be given in reverse
unwrapApplication :: Identifier -> Mono.Type -> [Mono.Exp] -> Monomorph Mono.Exp
unwrapApplication f returnty [] = return (Mono.Symbol f returnty)
unwrapApplication f returnty (arg:args) =
  do let ty = Mono.typeOf arg Mono.:> returnty
     rest <- unwrapApplication f ty args
     return (Mono.App rest arg returnty)

-- Convert body and list of parameters, into nested lambdas
unwrapLambdas :: Poly.Exp -> [(Identifier, Poly.Type)] -> Poly.Exp
unwrapLambdas body [] = body
unwrapLambdas body ((param,argty):ps) =
  let rest = unwrapLambdas body ps
      retty = Poly.typeOf rest
  in Poly.Lamb param rest (argty Poly.:> retty)

-- Create nested let's
unwrapAsLets :: [(Identifier, Mono.Exp)] -> Mono.Exp -> Mono.Exp
unwrapAsLets [] body = body
unwrapAsLets ((mn, me):is) body =
  Mono.Let mn me (unwrapAsLets is body) (Mono.typeOf body)

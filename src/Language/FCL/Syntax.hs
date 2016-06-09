module Language.FCL.Syntax (
  -- Types
  Level(..),
  threadLevel,
  warpLevel,
  blockLevel,
  gridLevel,
  LvlVar(..),
  Type(..),
  TyVar(..),
  TypeScheme(..),
  freeTyVars,
  Untyped(Untyped),

  -- Expressions
  Name,
  Exp(..),
  UnOp(..),
  BinOp(..),
  freeIn,
  freeVars,
  apply,
  typeOf,
  isScalar,

  -- Programs
  KernelConfig(..),
  defaultKernelConfig,
  Definition(..),
  Program,
  mapBody
) where

import Control.Monad.Trans.State
import qualified Data.Set as Set

import Language.FCL.SourceRegion

---------------------
-- Syntax of types --
---------------------
data LvlVar = LvlVar Int (Maybe String)
  deriving (Eq, Show, Ord)

threadLevel, warpLevel, blockLevel, gridLevel :: Level
threadLevel = Zero
warpLevel   = Step threadLevel
blockLevel  = Step warpLevel
gridLevel   = Step blockLevel

data Level = Zero | Step Level | VarL LvlVar
  deriving (Eq, Show)
           
data Type =
    IntT
  | BoolT
  | DoubleT
  | VarT TyVar
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  deriving (Eq, Show)

data TyVar = TyVar Int (Maybe String)
  deriving (Eq, Show, Ord)

data TypeScheme ty = TypeScheme [TyVar] ty
  deriving Show

-- | Return the list of type variables in t (possibly with duplicates)
freeTyVars :: Type -> [TyVar]
freeTyVars IntT       = []
freeTyVars BoolT       = []
freeTyVars DoubleT       = []
freeTyVars (t1 :> t2) = freeTyVars t1 ++ freeTyVars t2
freeTyVars (t1 :*: t2) = freeTyVars t1 ++ freeTyVars t2
freeTyVars (PullArrayT t) = freeTyVars t
freeTyVars (PushArrayT _ t) = freeTyVars t
freeTyVars (VarT v)  = [v]

data Untyped = Untyped
  deriving (Eq, Show)

---------------------------
-- Syntax of Expressions --
---------------------------
type Name = String

data Exp ty =
    IntScalar Int Region
  | DoubleScalar Double Region
  | BoolScalar Bool Region
  | UnOp UnOp (Exp ty) Region
  | BinOp BinOp (Exp ty) (Exp ty) Region
  | Var Name ty Region
  | Vec [Exp ty] ty Region
  | Lamb Name ty (Exp ty) ty Region
  | Let Name (Exp ty) (Exp ty) ty Region
  | App (Exp ty) (Exp ty)

  | Cond (Exp ty) (Exp ty) (Exp ty) ty Region
  | Pair (Exp ty) (Exp ty) Region
  | Proj1E (Exp ty) Region
  | Proj2E (Exp ty) Region

-- Array handling
  | Index (Exp ty) (Exp ty) Region
  | LengthPull (Exp ty) Region
  | LengthPush (Exp ty) Region

-- Combinators
  | While (Exp ty) (Exp ty) (Exp ty) Region -- APL-style, representing tail-recursive functions
  | WhileSeq (Exp ty) (Exp ty) (Exp ty) Region
  | GeneratePull (Exp ty) (Exp ty) Region
  | MapPull (Exp ty) (Exp ty) Region
  | MapPush (Exp ty) (Exp ty) Region
  | Force (Exp ty) Region
  | Push Level (Exp ty) ty Region
  | Concat (Exp ty) (Exp ty) Region
  | Interleave (Exp ty) (Exp ty) (Exp ty) Region
  | BlockSize Region

  -- Sequential scan, I don't really want this!
  | Scanl (Exp ty) (Exp ty) (Exp ty) Region
  deriving (Eq, Show)

  -- To be added later!
  -- | Replicate Exp Exp
  -- | Permute Exp Exp
  -- | Append Exp Exp

data UnOp = AbsI | SignI | NegateI | Not | I2D | B2I | CLZ
  deriving (Eq, Show)

data BinOp = AddI | SubI | MulI | DivI | ModI | MinI
           | EqI | NeqI | AndI | OrI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
  deriving (Eq, Show)

isScalar :: Exp ty -> Bool
isScalar (IntScalar _ _) = True
isScalar (DoubleScalar _ _) = True
isScalar (BoolScalar _ _) = True
isScalar _ = False

typeOf :: Exp Type -> Type
typeOf (IntScalar _ _) = IntT
typeOf (DoubleScalar _ _) = DoubleT
typeOf (BoolScalar _ _) = BoolT
typeOf (UnOp op _ _) =
    if op `elem` [ AbsI, SignI ]
    then IntT
    else error "typeOf: UnOp"
typeOf (BinOp op _ _ _) =
  if op `elem` [ AddI, SubI, MulI, DivI, ModI, MinI ]
    then IntT
    else
      if op `elem` [ EqI ]
        then BoolT
        else error "typeOf: BinOp"
typeOf (Var _ ty _) = ty
typeOf (Lamb _ ty0 _ ty1 _) = ty0 :> ty1
typeOf (Let _ _ _ ty _) = ty
typeOf (App e _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (Cond _ _ _ ty _) = ty
typeOf (Pair e0 e1 _) = (typeOf e0) :*: (typeOf e1)
typeOf (Proj1E e _) =
  case typeOf e of
    (t0 :*: _) -> t0
    _ -> error "typeOf: Argument to Proj1E not of product type"
typeOf (Proj2E e _) =
  case typeOf e of
    (_ :*: t1) -> t1
    _ -> error "typeOf: Argument to Proj2E not of product type"
typeOf (Index e0 _ _) =
  case typeOf e0 of
    PullArrayT elem_ty -> elem_ty
    _ -> error "typeOf: Argument to Index not of push-array type"
typeOf (LengthPull _ _) = IntT
typeOf (LengthPush _ _) = IntT
typeOf (While _ _ e2 _) = typeOf e2
typeOf (WhileSeq _ _ e2 _) = typeOf e2
typeOf (GeneratePull _ f _) =
  case typeOf f of
    _ :> ty1 -> PullArrayT ty1
    _ -> error "typeOf: Second argument to Generate not of function type"
typeOf (MapPull e0 e1 _) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, PullArrayT _) -> PullArrayT ty1
    _ -> error "typeOf: Map"
typeOf (MapPush e0 e1 _) =
  case (typeOf e0, typeOf e1) of
    (_ :> ty1, PushArrayT lvl _) -> PushArrayT lvl ty1
    _ -> error "typeOf: Map"
typeOf (Push _ _ ty _) = ty
typeOf (Force e0 _) =
  case (typeOf e0) of
    (PushArrayT _ ty0) -> PullArrayT ty0
    _ -> error "typeOf: Force"
typeOf (Concat _ e0 _) =
  case typeOf e0 of
    PullArrayT (PushArrayT lvl t) -> PushArrayT (Step lvl) t
    _ -> error "typeOf: Concat given non-push-array as third argument"
typeOf (Interleave _ _ e0 _) =
  case typeOf e0 of
    PullArrayT (PushArrayT lvl t) -> PushArrayT (Step lvl) t
    _ -> error "typeOf: Interleave given non-pull-array as third argument"
typeOf (Vec [] _ _) = error "Cannot type empty list"
typeOf (Vec es _ _) =
  let (t:ts) = map typeOf es
  in if and $ zipWith (==) (t:ts) ts
       then PullArrayT t
       else error "All elements in vector literal should be typed identically"
typeOf (BlockSize _) = IntT
typeOf (Scanl _ e1 e2 _) =
  case typeOf e2 of
    PullArrayT _ -> PushArrayT threadLevel (typeOf e1)
    _ -> error "typeOf: Scanl"

------------------------
-- Syntax of programs --
------------------------

type Program ty = [Definition ty]

data KernelConfig =
  KernelConfig
    { configBlockSize :: Maybe Int
    , configWarpSize  :: Int
    }
  deriving Show

defaultKernelConfig :: KernelConfig
defaultKernelConfig =
  KernelConfig
    { configBlockSize = Nothing
    , configWarpSize = 32
    }

data Definition ty =
  Definition
    { defVar        :: Name
    , defSignature  :: Maybe Type
    , defTypeScheme :: TypeScheme ty
    , defEmitKernel :: Bool
    , defKernelConfig :: KernelConfig
    , defBody       :: Exp ty
    }
  deriving Show

mapBody :: (Exp ty -> Exp ty)
        -> Definition ty -> Definition ty
mapBody f def = def { defBody = f (defBody def) }

------------------
-- Substitution --
------------------
freeIn :: Name -> Exp ty -> Bool
freeIn _ (IntScalar _ _)          = True
freeIn _ (DoubleScalar _ _)       = True
freeIn _ (BoolScalar _ _)         = True
freeIn x (UnOp _ e _)             = freeIn x e
freeIn x (BinOp _ e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (Var y _ _)              = x /= y
freeIn x (Vec es _ _)             = all (freeIn x) es
freeIn x (Lamb y _ e _ _)         = x == y || freeIn x e
freeIn x (Let y e ebody _ _)      = freeIn x e && (x == y || freeIn x ebody)
freeIn x (App e1 e2)              = freeIn x e1 && freeIn x e2
freeIn x (Cond e1 e2 e3 _ _)      = all (freeIn x) [e1, e2, e3]
freeIn x (LengthPull e _)         = freeIn x e
freeIn x (LengthPush e _)         = freeIn x e
freeIn x (Push _ e _ _)           = freeIn x e
freeIn x (Force e _)              = freeIn x e
freeIn x (Proj1E e _)             = freeIn x e
freeIn x (Proj2E e _)             = freeIn x e
freeIn x (Pair e1 e2 _)           = freeIn x e1 && freeIn x e2
freeIn x (Index e1 e2 _)          = freeIn x e1 && freeIn x e2
freeIn x (GeneratePull e1 e2 _)   = freeIn x e1 && freeIn x e2
freeIn x (MapPull e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (MapPush e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (Concat e1 e2 _)         = freeIn x e1 && freeIn x e2
freeIn x (Interleave e1 e2 e3 _)  = all (freeIn x) [e1, e2, e3]
freeIn x (While e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn x (WhileSeq e1 e2 e3 _)    = all (freeIn x) [e1, e2, e3]
freeIn x (Scanl e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn _ (BlockSize _)            = True

freeVars :: Exp ty -> Set.Set Name
freeVars (IntScalar _ _)          = Set.empty
freeVars (DoubleScalar _ _)       = Set.empty
freeVars (BoolScalar _ _)         = Set.empty
freeVars (UnOp _ e _)             = freeVars e
freeVars (BinOp _ e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (Var x _ _)              = Set.singleton x
freeVars (Vec es _ _)             = Set.unions (map freeVars es)
freeVars (Lamb x _ e _ _)         = Set.difference (freeVars e) (Set.singleton x)
freeVars (Let x e1 e2 _ _)        = Set.union (freeVars e1)
                                              (Set.difference (freeVars e2) (Set.singleton x))
freeVars (App e1 e2)              = Set.union (freeVars e1) (freeVars e2)
freeVars (Cond e1 e2 e3 _ _)      = Set.unions (map freeVars [e1, e2, e3])
freeVars (Force e _)              = freeVars e
freeVars (Push _ e _ _)           = freeVars e
freeVars (LengthPull e _)         = freeVars e
freeVars (LengthPush e _)         = freeVars e
freeVars (Proj1E e _)             = freeVars e
freeVars (Proj2E e _)             = freeVars e
freeVars (Pair e1 e2 _)           = Set.union (freeVars e1) (freeVars e2)
freeVars (Index e1 e2 _)          = Set.union (freeVars e1) (freeVars e2)
freeVars (GeneratePull e1 e2 _)   = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPull e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPush e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (Concat e1 e2 _)         = Set.union (freeVars e1) (freeVars e2)
freeVars (Interleave e1 e2 e3 _)  = Set.unions (map freeVars [e1, e2, e3])
freeVars (While e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (WhileSeq e1 e2 e3 _)    = Set.unions (map freeVars [e1, e2, e3])
freeVars (Scanl e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (BlockSize _)            = Set.empty

freshVar :: State [Name] Name
freshVar =
  do (x:xs) <- get
     put xs
     return x

subst :: Exp ty -> Name -> Exp ty -> State [Name] (Exp ty)
subst s x e =
  case e of
    (Var y _ _)
      | x == y    -> return s
      | otherwise -> return e
    (App e1 e2)   ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (App e1' e2')
    (Lamb y ty1 ebody ty2 r)
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             ebody' <- subst (Var z ty1 Missing) y ebody
             ebody'' <- subst s x ebody'
             return (Lamb z ty1 ebody'' ty2 r)
      | otherwise ->
          do ebody' <- subst s x ebody
             return (Lamb y ty1 ebody' ty2 r)
    (Let y e1 e2 t r)
      | x == y                    -> return e
      | Set.member y (freeVars s) ->
          do z <- freshVar
             e1' <- subst s x e1
             e2' <- subst (Var z t Missing) y e2 -- TODO the "t" here is REALLY wrong
             e2'' <- subst s x e2'
             return (Let z e1' e2'' t r)
      | otherwise ->
          do e1' <- subst s x e1
             e2' <- subst s x e2
             return (Let y e1' e2' t r)

   -- Recurse in all other cases
    IntScalar _ _ -> return e
    DoubleScalar _ _ -> return e
    BoolScalar _ _ -> return e
    UnOp op e1 r ->
       do e1' <- subst s x e1
          return (UnOp op e1' r)
    BinOp op e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (BinOp op e1' e2' r)
    Vec es ty r ->
      do es' <- mapM (subst s x) es
         return (Vec es' ty r)
    Cond e1 e2 e3 ty r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Cond e1' e2' e3' ty r)
    Pair e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Pair e1' e2' r)
    Proj1E e1 r ->
       do e1' <- subst s x e1
          return (Proj1E e1' r)
    Proj2E e1 r ->
       do e1' <- subst s x e1
          return (Proj2E e1' r)
    Index e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Index e1' e2' r)
    LengthPull e1 r ->
       do e1' <- subst s x e1
          return (LengthPull e1' r)
    LengthPush e1 r ->
       do e1' <- subst s x e1
          return (LengthPush e1' r)
    While e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (While e1' e2' e3' r)
    WhileSeq e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (WhileSeq e1' e2' e3' r)
    GeneratePull e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (GeneratePull e1' e2' r)
    MapPull e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (MapPull e1' e2' r)
    MapPush e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (MapPush e1' e2' r)
    Force e1 r ->
       do e1' <- subst s x e1
          return (Force e1' r)
    Push lvl e1 ty r ->
       do e1' <- subst s x e1
          return (Push lvl e1' ty r)
    Concat e1 e2 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          return (Concat e1' e2' r)
    Interleave e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Interleave e1' e2' e3' r)
    BlockSize _ -> return e
    Scanl e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Scanl e1' e2' e3' r)

apply :: (Name, Exp ty) -> Exp ty -> Exp ty
apply (x, ebody) e =
  let vars = ['x' : show (i :: Int) | i <- [0..]]
  in evalState (subst e x ebody) vars

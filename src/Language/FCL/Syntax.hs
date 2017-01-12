module Language.FCL.Syntax (
  -- Types
  Level(..),
  threadLevel,
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
  Definition(..),
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

threadLevel, blockLevel, gridLevel :: Level
threadLevel = Zero
blockLevel  = Step threadLevel
gridLevel   = Step blockLevel

data Level = Zero | Step Level | VarL LvlVar
  deriving (Eq, Show)
           
data Type =
    IntT
  | BoolT
  | DoubleT
  | StringT
  | UnitT
  | VarT TyVar
  | LvlVar :-> Type
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  | ProgramT Level Type
  deriving (Eq, Show)

data TyVar = TyVar Int (Maybe String)
  deriving (Eq, Show, Ord)

data TypeScheme ty = TypeScheme [TyVar] ty
  deriving (Eq, Show)

-- | Return the list of type variables in t (possibly with duplicates)
freeTyVars :: Type -> [TyVar]
freeTyVars IntT        = []
freeTyVars BoolT       = []
freeTyVars DoubleT     = []
freeTyVars StringT     = []
freeTyVars UnitT       = []
freeTyVars (_ :-> t)   = freeTyVars t
freeTyVars (t1 :> t2)  = freeTyVars t1 ++ freeTyVars t2
freeTyVars (t1 :*: t2) = freeTyVars t1 ++ freeTyVars t2
freeTyVars (PullArrayT t)   = freeTyVars t
freeTyVars (PushArrayT _ t) = freeTyVars t
freeTyVars (VarT v)         = [v]
freeTyVars (ProgramT _ t)   = freeTyVars t

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
  | String String Region
  | UnOp UnOp (Exp ty) Region
  | BinOp BinOp (Exp ty) (Exp ty) Region
  | Var Name ty Region
  | Vec [Exp ty] ty Region
  | Lamb Name ty (Exp ty) ty Region
  | Let Name (Exp ty) (Exp ty) ty Region
  | App (Exp ty) (Exp ty)

  | LambLvl LvlVar (Exp ty) ty Region
  | AppLvl (Exp ty) Level
    
  | Cond (Exp ty) (Exp ty) (Exp ty) ty Region
  | Pair (Exp ty) (Exp ty) Region
  | Proj1E (Exp ty) Region
  | Proj2E (Exp ty) Region

-- Array handling
  | Index (Exp ty) (Exp ty) Region
  | LengthPull (Exp ty) Region
  | LengthPush (Exp ty) Region

-- Combinators
  | For (Exp ty) (Exp ty) (Exp ty) Region -- Sequential for loop at thread-level
  | Power (Exp ty) (Exp ty) (Exp ty) Region -- APL-style power-operator
  | While (Exp ty) (Exp ty) (Exp ty) Region -- APL-style, representing tail-recursive functions
  | WhileSeq (Exp ty) (Exp ty) (Exp ty) Region
  | GeneratePull (Exp ty) (Exp ty) Region
  | MapPull (Exp ty) (Exp ty) Region
  | MapPush (Exp ty) (Exp ty) Region
  | Force (Exp ty) Region
  | Push Level (Exp ty) Region
  | Interleave (Exp ty) (Exp ty) (Exp ty) Region
  | Bind (Exp ty) (Exp ty) Region
  | Return Level (Exp ty) Region
  | BlockSize Region

-- I/O
  | ReadIntCSV (Exp ty) Region
  -- | ReadDoubleCSV (Exp ty)
  | ForceAndPrint (Exp ty) (Exp ty) Region
  | Benchmark (Exp ty) (Exp ty) Region
  -- | PrintDoubleArray (Exp ty) (Exp ty)
  deriving (Show)

equals :: Exp ty -> Exp ty -> Bool
equals (IntScalar i0 _) (IntScalar i1 _) = i0 == i1
equals (DoubleScalar d0 _) (DoubleScalar d1 _) = d0 == d1
equals (BoolScalar b0 _) (BoolScalar b1 _) = b0 == b1
equals (UnOp op0 x0 _) (UnOp op1 x1 _) = op0 == op1 && equals x0 x1
equals (BinOp op0 x0 y0 _) (BinOp op1 x1 y1 _) = op0 == op1 && equals x0 x1 && equals y0 y1
equals (Var n0 _ _) (Var n1 _ _) = n0 == n1
equals (Vec es0 _ _) (Vec es1 _ _) = and (zipWith equals es0 es1)
equals (Lamb n0 _ e0 _ _) (Lamb n1 _ e1 _ _) = n0 == n1 && equals e0 e1
equals (Let n0 e0 e1 _ _) (Let n1 e0' e1' _ _) = n0 == n1 && equals e0 e0' && equals e1 e1'
equals (App e0 e1) (App e0' e1') = equals e0 e0' && equals e1 e1'
equals (LambLvl lvl0 e0 _ _) (LambLvl lvl0' e0' _ _) = lvl0 == lvl0' && equals e0 e0'
equals (AppLvl e0 lvl0) (AppLvl e0' lvl0') = lvl0 == lvl0' && equals e0 e0'
equals (Cond e0 e1 e2 _ _) (Cond e0' e1' e2' _ _) = equals e0 e0' && equals e1 e1' && equals e2 e2'
equals (Pair e0 e1 _) (Pair e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (Proj1E e0 _) (Proj1E e0' _) = equals e0 e0'
equals (Proj2E e0 _) (Proj2E e0' _) = equals e0 e0'

equals (Index e0 e1 _) (Index e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (LengthPull e0 _) (LengthPull e0' _) = equals e0 e0'
equals (LengthPush e0 _) (LengthPush e0' _) = equals e0 e0'

equals (Power e0 e1 e2 _) (Power e0' e1' e2' _) = equals e0 e0' && equals e1 e1' && equals e2 e2'
equals (While e0 e1 e2 _) (While e0' e1' e2' _) = equals e0 e0' && equals e1 e1' && equals e2 e2'
equals (WhileSeq e0 e1 e2 _) (WhileSeq e0' e1' e2' _) = equals e0 e0' && equals e1 e1' && equals e2 e2'
equals (GeneratePull e0 e1 _) (GeneratePull e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (MapPull e0 e1 _) (MapPull e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (MapPush e0 e1 _) (MapPush e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (Force e0 _) (Force e0' _) = equals e0 e0'
equals (Push lvl0 e0 _) (Push lvl0' e0' _) = lvl0 == lvl0' && equals e0 e0'
equals (Interleave e0 e1 e2 _) (Interleave e0' e1' e2' _) = equals e0 e0' && equals e1 e1' && equals e2 e2'
equals (Return lvl0 e0 _) (Return lvl0' e0' _) = lvl0 == lvl0' && equals e0 e0'
equals (Bind e0 e1 _) (Bind e0' e1' _) = equals e0 e0' && equals e1 e1'
equals (BlockSize _) (BlockSize _) = True
equals _ _ = False

instance Eq (Exp ty) where
  (==) = equals

data UnOp = AbsI | SignI | NegateI | Not | I2D | B2I | CLZ
  deriving (Eq, Show)

data BinOp = AddI | SubI | MulI | DivI | ModI | MinI | MaxI
           | EqI | NeqI | LtI | AndI | OrI | XorI | ShiftLI | ShiftRI
           | PowI | DivR | PowR
           | AddR
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
typeOf (String _ _) = StringT
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
typeOf (App e _) =
  case typeOf e of
    _ :> ty1 -> ty1
    _ -> error "typeOf: First argument to App not of function type"
typeOf (LambLvl lvlvar _ ty _) = lvlvar :-> ty
typeOf (AppLvl e _) =
  case typeOf e of
    _ :-> ty -> ty
    _ -> error "typeOf: First argument to AppLvl not of function type"
typeOf (Let _ _ _ ty _) = ty
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
typeOf (Power _ _ e2 _) =
  case typeOf e2 of
    (ProgramT lvl (PushArrayT _ ty)) -> ProgramT lvl (PullArrayT ty)
    _ -> error "typeOf: Argument to Power not of Program lvl push-array type."
typeOf (For _ _ e2 _) =
  case typeOf e2 of
    ((_ :> ty) :> _) -> ty
    _ -> error "typeOf: Argument to For not of correct type."
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
typeOf (Push lvl e0 _) =
  case typeOf e0 of
    PullArrayT ty -> PushArrayT lvl ty
    _ -> error "typeOf: push"
typeOf (Force e0 _) =
  case (typeOf e0) of
    (PushArrayT _ ty0) -> PullArrayT ty0
    _ -> error "typeOf: Force"
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
typeOf (Return lvl e0 _) =
  ProgramT lvl (typeOf e0)
typeOf (Bind _ e1 _) =
  case typeOf e1 of
    (_ :> ty) -> ty
    _ -> error "typeOf: bind"
typeOf (ReadIntCSV _ _) = ProgramT gridLevel (PullArrayT StringT)
typeOf (ForceAndPrint _ _ _) = ProgramT gridLevel UnitT
typeOf (Benchmark _ _ _) = ProgramT gridLevel UnitT

------------------------
-- Syntax of programs --
------------------------

data Definition ty =
  Definition
    { defVar        :: Name
    , defSignature  :: Maybe Type
    , defTypeScheme :: TypeScheme ty
    , defEmitKernel :: Bool
    , defBody       :: Exp ty
    }
  deriving (Show, Eq)

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
freeIn _ (String _ _)             = True
freeIn x (UnOp _ e _)             = freeIn x e
freeIn x (BinOp _ e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (Var y _ _)              = x /= y
freeIn x (Vec es _ _)             = all (freeIn x) es
freeIn x (Lamb y _ e _ _)         = x == y || freeIn x e
freeIn x (App e1 e2)              = freeIn x e1 && freeIn x e2
freeIn x (LambLvl _ e _ _)        = freeIn x e
freeIn x (AppLvl e _)             = freeIn x e
freeIn x (Let y e ebody _ _)      = freeIn x e && (x == y || freeIn x ebody)
freeIn x (Cond e1 e2 e3 _ _)      = all (freeIn x) [e1, e2, e3]
freeIn x (LengthPull e _)         = freeIn x e
freeIn x (LengthPush e _)         = freeIn x e
freeIn x (Push _ e _)             = freeIn x e
freeIn x (Force e _)              = freeIn x e
freeIn x (Proj1E e _)             = freeIn x e
freeIn x (Proj2E e _)             = freeIn x e
freeIn x (Pair e1 e2 _)           = freeIn x e1 && freeIn x e2
freeIn x (Index e1 e2 _)          = freeIn x e1 && freeIn x e2
freeIn x (GeneratePull e1 e2 _)   = freeIn x e1 && freeIn x e2
freeIn x (MapPull e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (MapPush e1 e2 _)        = freeIn x e1 && freeIn x e2
freeIn x (Interleave e1 e2 e3 _)  = all (freeIn x) [e1, e2, e3]
freeIn x (For e1 e2 e3 _)         = all (freeIn x) [e1, e2, e3]
freeIn x (Power e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn x (While e1 e2 e3 _)       = all (freeIn x) [e1, e2, e3]
freeIn x (WhileSeq e1 e2 e3 _)    = all (freeIn x) [e1, e2, e3]
freeIn _ (BlockSize _)            = True
freeIn x (Return _ e _)           = freeIn x e
freeIn x (Bind e1 e2 _)           = freeIn x e1 && freeIn x e2
freeIn x (ForceAndPrint e1 e2 _)  = freeIn x e1 && freeIn x e2
freeIn x (Benchmark e1 e2 _)  = freeIn x e1 && freeIn x e2
freeIn x (ReadIntCSV e1 _)        = freeIn x e1

freeVars :: Exp ty -> Set.Set Name
freeVars (IntScalar _ _)          = Set.empty
freeVars (DoubleScalar _ _)       = Set.empty
freeVars (BoolScalar _ _)         = Set.empty
freeVars (String _ _)             = Set.empty
freeVars (UnOp _ e _)             = freeVars e
freeVars (BinOp _ e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (Var x _ _)              = Set.singleton x
freeVars (Vec es _ _)             = Set.unions (map freeVars es)
freeVars (Lamb x _ e _ _)         = Set.difference (freeVars e) (Set.singleton x)
freeVars (App e1 e2)              = Set.union (freeVars e1) (freeVars e2)
freeVars (LambLvl _ e _ _)        = freeVars e
freeVars (AppLvl e _)             = freeVars e
freeVars (Let x e1 e2 _ _)        = Set.union (freeVars e1)
                                              (Set.difference (freeVars e2) (Set.singleton x))
freeVars (Cond e1 e2 e3 _ _)      = Set.unions (map freeVars [e1, e2, e3])
freeVars (Force e _)              = freeVars e
freeVars (Push _ e _)             = freeVars e
freeVars (LengthPull e _)         = freeVars e
freeVars (LengthPush e _)         = freeVars e
freeVars (Proj1E e _)             = freeVars e
freeVars (Proj2E e _)             = freeVars e
freeVars (Pair e1 e2 _)           = Set.union (freeVars e1) (freeVars e2)
freeVars (Index e1 e2 _)          = Set.union (freeVars e1) (freeVars e2)
freeVars (GeneratePull e1 e2 _)   = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPull e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (MapPush e1 e2 _)        = Set.union (freeVars e1) (freeVars e2)
freeVars (Interleave e1 e2 e3 _)  = Set.unions (map freeVars [e1, e2, e3])
freeVars (For e1 e2 e3 _)         = Set.unions (map freeVars [e1, e2, e3])
freeVars (Power e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (While e1 e2 e3 _)       = Set.unions (map freeVars [e1, e2, e3])
freeVars (WhileSeq e1 e2 e3 _)    = Set.unions (map freeVars [e1, e2, e3])
freeVars (BlockSize _)            = Set.empty
freeVars (Return _ e _)           = freeVars e
freeVars (Bind e1 e2 _)           = Set.union (freeVars e1) (freeVars e2)
freeVars (ForceAndPrint e1 e2 _)  = Set.union (freeVars e1) (freeVars e2)
freeVars (Benchmark e1 e2 _)  = Set.union (freeVars e1) (freeVars e2)
freeVars (ReadIntCSV e1 _)        = freeVars e1

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
    (AppLvl e0 lvl)   ->
          do e0' <- subst s x e0
             return (AppLvl e0' lvl)
    (LambLvl lvlvar ebody ty r) ->
           do ebody' <- subst s x ebody
              return (LambLvl lvlvar ebody' ty r)
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
    String _ _ -> return e
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
    For e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (For e1' e2' e3' r)
    Power e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Power e1' e2' e3' r)
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
    Push lvl e1 r ->
       do e1' <- subst s x e1
          return (Push lvl e1' r)
    Interleave e1 e2 e3 r ->
       do e1' <- subst s x e1
          e2' <- subst s x e2
          e3' <- subst s x e3
          return (Interleave e1' e2' e3' r)
    BlockSize _ -> return e
    Return lvl e1 r ->
      do e1' <- subst s x e1
         return (Return lvl e1' r)
    Bind e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (Bind e1' e2' r)
    ForceAndPrint e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (ForceAndPrint e1' e2' r)
    Benchmark e1 e2 r ->
      do e1' <- subst s x e1
         e2' <- subst s x e2
         return (Benchmark e1' e2' r)
    ReadIntCSV e1 r ->
      do e1' <- subst s x e1
         return (ReadIntCSV e1' r)

apply :: (Name, Exp ty) -> Exp ty -> Exp ty
apply (x, ebody) e =
  let vars = ['x' : show (i :: Int) | i <- [0..]]
  in evalState (subst e x ebody) vars

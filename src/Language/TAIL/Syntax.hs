module Language.TAIL.Syntax where

-----------
-- Types --
-----------
type RankVar = Int
type BaseVar = Int
type TyVar = Int

type Name = String

data BaseType = IntT
              | DoubleT
              | BoolT
              | CharT
              | BaseVar BaseVar
  deriving (Eq, Show)

data Rank = Rank Int
          | RankAdd Rank Rank
          | RankVar RankVar

data Type = ArrayT BaseType Rank
          | VectorT BaseType Rank
          | SingleT BaseType Rank    -- ^ Type of Singleton (0D array, i.e. base value)
          | SingleVecT BaseType Rank -- ^ Type of Singleton 1D-vector
          | Type :> Type
          | TyVarT Int

data TypeScheme ty = TypeScheme ([TyVar], [RankVar], [BaseVar]) ty
  deriving Show

scalarTy :: BaseType -> Type
scalarTy bt = ArrayT bt (Rank 0)

vectorTy :: BaseType -> Type
vectorTy bt = ArrayT bt (Rank 1)

mapBaseType :: (BaseType -> BaseType) -> Type -> Type
mapBaseType f (ArrayT b r) = ArrayT (f b) r
mapBaseType f (VectorT b r) = VectorT (f b) r
mapBaseType f (SingleT b r) = SingleT (f b) r
mapBaseType f (SingleVecT b r) = SingleVecT (f b) r
mapBaseType _ _ = error "Cannot be applied"

basetype :: Type -> Maybe BaseType
basetype t =
  case t of
    ArrayT bty _     -> Just bty
    VectorT bty _    -> Just bty
    SingleT bty _    -> Just bty
    SingleVecT bty _ -> Just bty
    _ :> _    -> Nothing
    TyVarT _         -> Nothing

-- | Return the list of type variables in t (possibly with duplicates)
freeTyVars :: Type -> [TyVar]
freeTyVars (t1 :> t2) = freeTyVars t1 ++ freeTyVars t2
freeTyVars (TyVarT v)  = [v]
freeTyVars _ = []

freeBaseVars :: Type -> [BaseVar]
freeBaseVars t =
  case t of
    ArrayT (BaseVar v) _     -> [v]
    VectorT (BaseVar v) _    -> [v]
    SingleT (BaseVar v) _    -> [v]
    SingleVecT (BaseVar v) _ -> [v]
    (t1 :> t2)               -> freeBaseVars t1 ++ freeBaseVars t2
    _         -> []

freeRankVars :: Type -> [RankVar]
freeRankVars t =
  case t of
    ArrayT _ (RankVar v)     -> [v]
    VectorT _ (RankVar v)    -> [v]
    SingleT _ (RankVar v)    -> [v]
    SingleVecT _ (RankVar v) -> [v]
    (t1 :> t2)               -> freeRankVars t1 ++ freeRankVars t2
    _         -> []

-----------------
-- Expressions --
-----------------
data UnaryOp = Ceil | Floor | Exp | Ln | 
               SignInt | AbsoluteInt | SignDouble | AbsoluteDouble |
               Cos | Sin | Tan | Acos | Asin | Atan | 
               Cosh | Sinh | Tanh | Pi | 
               Not | I2D | B2I | NegateInt | NegateDouble
  deriving (Eq, Show)

data BinOp   = AddI | SubI | MulI | DivI | 
               AddD | SubD | MulD | DivD | PowD |
               LtI | LteI | GtI | GteI | EqI | NeqI | 
               LtD | LteD | GtD | GteD | EqD | NeqD | 
               LtC | LteC | GtC | GteC | EqC | NeqC | 
               MaxI | MinI | 
               MaxD | MinD | 
               And | Or | Xor | Nand | Nor
  deriving (Eq, Show)

data ScalarValue =
    IntV Int
  | BoolV Bool
  | DoubleV Double
  | CharV Char
  deriving (Eq, Show)

data Untyped = Untyped
  deriving (Eq, Show)

data Exp ty = 
    Scalar ScalarValue
  | Var Name ty
  | Lam Name Type (Exp ty) ty
  | Let Name (Exp ty) (Exp ty) ty
  | Vector [Exp ty] ty

  | UnaryOp UnaryOp (Exp ty)
  | BinOp BinOp (Exp ty) (Exp ty)
  | Infinity   ty
  | ZipWith    (Exp ty) (Exp ty) (Exp ty) ty
  | Each       (Exp ty) (Exp ty) ty
  | Catenate   (Exp ty) (Exp ty) ty

  | If (Exp ty) (Exp ty) (Exp ty) ty
  | TypeAnnotation (Exp ty) Type

  | Reduce     (Exp ty) (Exp ty) (Exp ty) ty
  | Foldl      (Exp ty) (Exp ty) ty
  | Scan       (Exp ty) (Exp ty) ty
  | Reshape    (Exp ty) (Exp ty) ty
  | Ravel      (Exp ty) ty
  | Transpose  (Exp ty) ty
  | Transpose2 (Exp ty) (Exp ty) ty

  | Drop       (Exp ty) (Exp ty) ty
  | Take       (Exp ty) (Exp ty) ty
  | Iota       (Exp ty) ty
  | Shape      (Exp ty) ty
  | Cons       (Exp ty) (Exp ty) ty
  | Snoc       (Exp ty) (Exp ty) ty
  | Rotate     (Exp ty) (Exp ty) ty

  | EachV      (Exp ty) (Exp ty) ty
  | CatenateV  (Exp ty) (Exp ty) ty
  | DropV      (Exp ty) (Exp ty) ty
  | TakeV      (Exp ty) (Exp ty) ty
  | IotaV      (Exp ty) ty
  | ShapeV     (Exp ty) ty
  | ConsV      (Exp ty) (Exp ty) ty
  | SnocV      (Exp ty) (Exp ty) ty
  | RotateV    (Exp ty) (Exp ty) ty

true, false :: Exp e
true = Scalar (BoolV True)
false = Scalar (BoolV False)

intV :: Int -> Exp e
intV v = Scalar (IntV v)

doubleV :: Double -> Exp e
doubleV v = Scalar (DoubleV v)

charV :: Char -> Exp e
charV v = Scalar (CharV v)

typeOf :: Exp Type -> Type
typeOf (Scalar (IntV v))      = SingleT IntT (Rank v)
typeOf (Scalar (BoolV True))  = SingleT BoolT (Rank 1)
typeOf (Scalar (BoolV False)) = SingleT BoolT (Rank 0)
typeOf (Scalar (DoubleV _))   = VectorT DoubleT (Rank 0)
typeOf (Scalar (CharV _))     = VectorT CharT (Rank 0)
typeOf (Var _ t)              = t
typeOf (Lam _ _ _ t)        = t
typeOf (Let _ _ _ t)        = t
typeOf (Vector _ t)           = t
typeOf (If _ _ _ t)           = t
typeOf (TypeAnnotation e _) = typeOf e

typeOf (Take _ _ t)           = t
typeOf (Each _ _ t)           = t
typeOf (Reduce _ _ _ t)       = t
typeOf _ = error "typeOf: case not defined"

defaultElemFromType :: Type -> ScalarValue
defaultElemFromType t =
  case basetype t of
    Just IntT -> IntV 0
    Just DoubleT -> DoubleV 0.0
    Just BoolT -> BoolV False
    Just CharT -> CharV ' '
    Just (BaseVar v) -> error "Unresolved base-type variable: ++ " v
    Nothing -> error "defaultElem"

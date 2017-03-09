module FCL.Core.Polytyped where

import FCL.Core.Identifier
import FCL.Core.PolyLevel
import FCL.Core.Literal

data Type =
    IntT
  | BoolT
  | DoubleT
  | StringT
  | UnitT
  | VarT TyVar
  | Type :> Type
  | Type :*: Type
  | PullArrayT Type
  | PushArrayT Level Type
  | ProgramT Level Type
  deriving (Eq, Ord, Show)

infixr 9 :>

data TyVar = TyVar Int (Maybe Identifier)
  deriving (Eq, Ord, Show)

data TypeScheme = TypeScheme [LvlVar] [TyVar] Type
  deriving (Eq, Ord, Show)

data Exp =
    Literal Literal Type
  | Unit Type
  | Symbol Identifier [Level] Type
  | Lamb Identifier Exp Type
  | App Exp Exp Type
  | Pair Exp Exp Type
  | Cond Exp Exp Exp Type
  | Let Identifier TypeScheme [LvlVar] Exp Exp Type
  deriving Show

literalType :: Literal -> Type
literalType (LiteralInt _) = IntT
literalType (LiteralBool _) = BoolT
literalType (LiteralDouble _) = DoubleT
literalType (LiteralString _) = StringT

typeOf :: Exp -> Type
typeOf l =
  case l of
    (Literal _ ty)       -> ty
    (Unit ty)            -> ty
    (Symbol _ _ ty)      -> ty
    (Lamb _ _ ty)        -> ty
    (App _ _ ty)         -> ty
    (Pair _ _ ty)        -> ty
    (Cond _ _ _ ty)      -> ty
    (Let _ _ _ _ _ ty)   -> ty

isPolymorphicType :: Type -> Bool
isPolymorphicType t =
  case t of
   VarT _            -> True
   IntT              -> False
   BoolT             -> False
   DoubleT           -> False
   StringT           -> False
   UnitT             -> False
   (p :> r)          -> isPolymorphicType p || isPolymorphicType r
   (t0 :*: t1)       -> isPolymorphicType t0 || isPolymorphicType t1
   PullArrayT t0     -> isPolymorphicType t0
   PushArrayT lvl t0 -> isPolymorphicLevel lvl || isPolymorphicType t0
   ProgramT lvl t0   -> isPolymorphicLevel lvl || isPolymorphicType t0

encodeLevel :: Level -> String
encodeLevel (VarL i) = "lvl" ++ show i
encodeLevel Zero = "Z"
encodeLevel (Step lvl) = "S" ++ encodeLevel lvl ++ ""

encodeType :: Type -> String
encodeType (VarT i) = "a" ++ show i
encodeType IntT = "int"
encodeType BoolT = "bool"
encodeType DoubleT = "double"
encodeType StringT = "string"
encodeType UnitT = "unit"
encodeType (tl :> tr) =
  "_" ++ encodeType tl ++ "." ++ encodeType tr ++ "_"
encodeType (tl :*: tr) =
  "_" ++ encodeType tl ++ "*" ++ encodeType tr ++ "_"
encodeType (PullArrayT t) =
  "pull_" ++ encodeType t ++ "_endpull"
encodeType (PushArrayT lvl t) =
  "push_" ++ encodeLevel lvl ++ "_" ++ encodeType t ++ "_endpush"
encodeType (ProgramT lvl t) =
  "program_" ++ encodeLevel lvl ++ "_" ++ encodeType t ++ "_endprogram"

encodeDef :: Identifier -> Type -> String
encodeDef ident ty = ident ++ "__" ++ encodeType ty

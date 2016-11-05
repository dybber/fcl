module Language.FCL.Values where

import Language.FCL.KernelProgram
import Language.FCL.Syntax
import CGen

type Writer a = a -> CExp -> ILKernel ()

data Array a = ArrPull CExp Type (CExp -> a)
             | ArrPush CExp Level Type (Writer a -> ILKernel ())

mapArray :: (a -> b) -> Type -> Array a -> Array b
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush len lvl _ g) =
  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))

size :: Array a -> CExp
size (ArrPull len _ _)       = len
size (ArrPush len _ _ _)       = len

baseType :: Array a -> Type
baseType (ArrPull _ (PullArrayT bty) _) = bty
baseType (ArrPull _ bty _)              = bty
baseType (ArrPush _ _ bty _)      = bty

createPull :: VarName -> Type -> CExp -> Array Value
createPull name ty n = ArrPull n ty (\i -> tagExp ty (name ! i))

data Value = TagInt CExp
            | TagBool CExp
            | TagDouble CExp
            | TagArray (Array Value)
            | TagFn (Value -> Value)
            | TagPair Value Value
            | TagProgram (ILKernel Value)

instance Show (Array a) where
  show (ArrPull _ ty _) = show ty
  show (ArrPush _ _ ty _) = show ty

instance Show Value where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagArray e) = "TagArray: " ++ show e
  show (TagFn _) = "TagFn"
  show (TagPair e0 e1) = "TagPair(" ++ show e0 ++ ", " ++ show e1 ++ ")"
  show (TagProgram _) = "TagKernelProgram"

tagExp :: Type -> CExp -> Value
tagExp IntT e    = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e   = TagDouble e
tagExp t _       = error ("tagExp: " ++ show t)

unInt :: Value -> CExp
unInt (TagInt i) = i
unInt _          = error "expected int"

unBool :: Value -> CExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unArray :: Value -> Array Value
unArray (TagArray e) = e
unArray _            = error "expected array"

forceTo :: Writer Value -> Array Value -> ILKernel ()
forceTo writer (ArrPush _ _ _ m) = m writer
forceTo _ _ = error "err"

convertType :: Type -> CType
convertType IntT              = int32_t
convertType DoubleT           = double_t
convertType BoolT             = bool_t
convertType (PullArrayT ty)   = pointer_t [] (convertType ty)
convertType (PushArrayT _ ty) = pointer_t [] (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :-> _)         = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _)          = error "convertType: All type variables should have been resolved by now"
convertType (ProgramT _ _)    = error "convertType: cannot convert Program type"

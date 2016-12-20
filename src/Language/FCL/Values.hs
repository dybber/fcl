module Language.FCL.Values where

import Language.FCL.Syntax
import Language.FCL.IL.Cons

type Writer a = a -> ILExp -> Program ()

data Array a = ArrPull ILExp Type (ILExp -> a)
             | ArrPush ILExp Level Type (Writer a -> Program ())

mapArray :: (a -> b) -> Type -> Array a -> Array b
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush len lvl _ g) =
  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))

size :: Array a -> ILExp
size (ArrPull len _ _)       = len
size (ArrPush len _ _ _)       = len

baseType :: Array a -> Type
baseType (ArrPull _ (PullArrayT bty) _) = bty
baseType (ArrPull _ bty _)              = bty
baseType (ArrPush _ _ bty _)      = bty

createPull :: ILName -> Type -> ILExp -> Array Value
createPull name ty n = ArrPull n ty (\i -> tagExp ty (name ! i))

-- type ProgramThread a = (ILKernel a)
-- type ProgramWarp a  = ILKernel a
-- type ProgramBlock a  = ILKernel a
-- type ProgramGrid a = (ILKernel a, ILHost a, [TopLevel])

data Value = TagInt ILExp
           | TagBool ILExp
           | TagDouble ILExp
           | TagArray (Array Value)
           | TagFn (Value -> Value)
           | TagPair Value Value
           | TagProgram (Program Value)

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
  show (TagProgram _) = "TagProgram"

tagExp :: Type -> ILExp -> Value
tagExp IntT e    = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e   = TagDouble e
tagExp t _       = error ("tagExp: " ++ show t)

unInt :: Value -> ILExp
unInt (TagInt i) = i
unInt _          = error "expected int"

unBool :: Value -> ILExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unArray :: Value -> Array Value
unArray (TagArray e) = e
unArray _            = error "expected array"

forceTo :: Writer Value -> Array Value -> Program ()
forceTo writer (ArrPush _ _ _ m) = m writer
forceTo _ _ = error "err"

convertType :: Type -> ILType
convertType IntT              = ILInt
convertType DoubleT           = ILDouble
convertType BoolT             = ILBool
convertType (PullArrayT ty)   = ILArray (convertType ty)
convertType (PushArrayT _ ty) = ILArray (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :-> _)         = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _)          = error "convertType: All type variables should have been resolved by now"
convertType (ProgramT _ _)    = error "convertType: cannot convert Program type"


convertLevel :: Level -> ILLevel
convertLevel Zero               = Thread
convertLevel (Step Zero)        = Block
convertLevel (Step (Step Zero)) = Grid

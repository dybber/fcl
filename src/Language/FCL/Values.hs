module Language.FCL.Values where

import Language.FCL.Syntax
import Language.FCL.IL.Cons

type Writer a = a -> ILExp -> Program ()

data Array = ArrPull ILExp Type (ILExp -> Value)
           | ArrPush ILExp Level Type (Writer Value -> Program ())
           | ArrPushThread ILExp ILExp Type ((ILExp -> Value) -> ILExp -> (ILExp,Value)) (Maybe (Value -> Value))

mapArray :: (Value -> Value) -> Type -> Array -> Array
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush len lvl _ g) =
  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))
mapArray f outType (ArrPushThread size' iterations _ g Nothing) =
  ArrPushThread size' iterations outType g (Just f)
mapArray f outType (ArrPushThread len iterations _ g (Just h)) =
  ArrPushThread len iterations outType g (Just (f . h))

size :: Array -> ILExp
size (ArrPull len _ _)       = len
size (ArrPush len _ _ _)       = len
size (ArrPushThread len _ _ _ _) = len

baseType :: Array -> Type
baseType (ArrPull _ (PullArrayT bty) _) = bty -- TODO: this doesn't seem right.. recurse.
baseType (ArrPull _ bty _)              = bty
baseType (ArrPush _ _ bty _)      = bty
baseType (ArrPushThread _ _ bty _ _) = bty
  
createPull :: ILName -> Type -> ILExp -> Array
createPull name ty n = ArrPull n ty (\i -> tagExp ty (name ! i))

data Value = TagInt ILExp
           | TagBool ILExp
           | TagDouble ILExp
           | TagString ILExp
           | TagArray Array
           | TagFn (Value -> Value)
           | TagPair Value Value
           | TagProgram (Program Value)

instance Show Array where
  show (ArrPull _ ty _) = show ty
  show (ArrPush _ _ ty _) = show ty
  show (ArrPushThread _ _ ty _ _) = show ty

instance Show Value where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagString e) = "TagString(" ++ show e ++ ")"
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

app1 :: Value -> Value -> Value
app1 (TagFn f) x = f x
app1 _ _         = error "Unexpected value at function position in application"

app2 :: Value -> Value -> Value -> Value
app2 (TagFn f) x y = app1 (f x) y
app2 _ _ _         = error "expected function"

unBool :: Value -> ILExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unString :: Value -> ILExp
unString (TagString e) = e
unString _           = error "expected string"

unArray :: Value -> Array
unArray (TagArray e) = e
unArray _            = error "expected array"

convertType :: Type -> ILType
convertType IntT              = ILInt
convertType DoubleT           = ILDouble
convertType BoolT             = ILBool
convertType StringT           = ILString
convertType (PullArrayT ty)   = ILArray (convertType ty)
convertType (PushArrayT _ ty) = ILArray (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :-> _)         = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _)          = error "convertType: All type variables should have been resolved by now"
convertType (ProgramT _ _)    = error "convertType: cannot convert Program type"
convertType UnitT             = error "convertType: cannot convert unit type"


convertLevel :: Level -> ILLevel
convertLevel Zero               = Thread
convertLevel (Step Zero)        = Block
convertLevel (Step (Step Zero)) = Grid
convertLevel _ = error "can not convert level"

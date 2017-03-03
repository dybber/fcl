module FCL.Compile.Value where

import FCL.Core.Monotyped
import FCL.Core.MonoLevel
import FCL.IL.Cons

import FCL.Core.SourceRegion

type Writer a = a -> ILExp -> Program ()

data Array = ArrPull ILExp Type (ILExp -> Value)
           | ArrPush Level Array
           | ArrPushThread ILExp ILExp Type ((ILExp -> Value) -> ILExp -> (ILExp,Value)) (Maybe (Value -> Value))
           | ArrInterleave Level ILExp (Value -> Value) Array

mapArray :: (Value -> Value) -> Type -> Array -> Array
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush lvl arr) = ArrPush lvl (mapArray f outType arr)
--  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))
mapArray f outType (ArrPushThread size' iterations _ g Nothing) =
  ArrPushThread size' iterations outType g (Just f)
mapArray f outType (ArrPushThread len iterations _ g (Just h)) =
  ArrPushThread len iterations outType g (Just (f . h))
mapArray f outType (ArrInterleave lvl n ixt arr) = ArrInterleave lvl n ixt (mapArray f outType arr)

size :: Array -> ILExp
size (ArrPull len _ _)       = len
size (ArrPush _ arr)       = size arr
size (ArrPushThread len _ _ _ _) = len
size (ArrInterleave _ rn _ arr) = rn `muli` (size arr)

baseType :: Array -> Type
baseType (ArrPull _ ty _)              = elemType ty
baseType (ArrPush _ arr)      = baseType arr
baseType (ArrPushThread _ _ bty _ _) = bty
baseType (ArrInterleave _ _ _ arr) = baseType arr

elemType :: Type -> Type
elemType IntT = IntT
elemType DoubleT = DoubleT
elemType BoolT = BoolT
elemType (PullArrayT ty) = elemType ty
elemType (PushArrayT _ ty) = elemType ty
elemType (ProgramT _ ty) = elemType ty
elemType t = error ("elemType: non array type found: " ++ show t)

createPull :: ILName -> Type -> ILExp -> Array
createPull name ty n = ArrPull n ty (\i -> tagScalar ty (name ! i))

data Value = TagInt ILExp
           | TagBool ILExp
           | TagDouble ILExp
           | TagString ILExp
           | TagArray Array
           | TagUnit
           | TagFn (Value -> Value)
           | TagPair Value Value
           | TagProgram (Program Value)

instance Show Array where
  show (ArrPull _ ty _) = "pull<" ++ show ty ++ ">"
  show (ArrPush lvl arr) = "push<" ++ show lvl ++ "> (" ++ show arr ++ ")"
  show (ArrInterleave lvl e _ arr) = "interleave<" ++ show lvl ++ "> (" ++ show e ++ ") <fn> (" ++ show arr ++ ")"
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
  show TagUnit = "TagUnit"

untagScalar :: Type -> Value -> ILExp
untagScalar IntT (TagInt e) = e
untagScalar DoubleT (TagDouble e) = e
untagScalar BoolT (TagBool e) = e
untagScalar _ _ = error ""

tagScalar :: Type -> ILExp -> Value
tagScalar IntT e = TagInt e
tagScalar DoubleT e = TagDouble e
tagScalar BoolT e = TagBool e
tagScalar _ _ = error ""

unop :: (Value -> Value) -> Value
unop f = TagFn (\a1 -> f a1)

binop :: (Value -> Value -> Value) -> Value
binop f = TagFn (\a1 -> (TagFn (\a2 -> f a1 a2)))

triop :: (Value -> Value -> Value -> Value) -> Value
triop f =
  TagFn (\a1 -> (
     TagFn (\a2 -> (
        TagFn (\a3 ->
           f a1 a2 a3)))))

app1 :: Value -> Value -> Value
app1 (TagFn f) x = f x
app1 _ _         = error "Unexpected value at function position in application"

app2 :: Value -> Value -> Value -> Value
app2 (TagFn f) x y = app1 (f x) y
app2 _ _ _         = error "expected function"

unInt :: Value -> ILExp
unInt (TagInt i) = i
unInt _          = error "expected int"

unDouble :: Value -> ILExp
unDouble (TagDouble d) = d
unDouble _          = error "expected double"

unBool :: Value -> ILExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unString :: Value -> ILExp
unString (TagString e) = e
unString _           = error "expected string"

unArray :: String -> Value -> Array
unArray _ (TagArray arr) = arr
unArray loc v            = error ("expected array in " ++ loc ++ " got " ++ show v)

unFn :: String -> Value -> (Value -> Value)
unFn _ (TagFn arr) = arr
unFn loc v            = error ("expected array in " ++ loc ++ " got " ++ show v)

unProgram :: String -> Value -> Program Value
unProgram _ (TagProgram p) = p
unProgram loc v            = error ("expected Program in " ++ loc ++ " got " ++ show v)

unPair :: String -> SourceRegion -> Value -> (Value, Value)
unPair _ _ (TagPair v1 v2) = (v1, v2)
unPair loc reg v           = error ("expected pair in " ++ loc ++ " at " ++ show reg ++ " got " ++ show v)

convertType :: Type -> ILType
convertType IntT              = ILInt
convertType DoubleT           = ILDouble
convertType BoolT             = ILBool
convertType StringT           = ILString
convertType (PullArrayT ty)   = ILArray (convertType ty)
convertType (PushArrayT _ ty) = ILArray (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (ProgramT _ _)    = error "convertType: cannot convert Program type"
convertType UnitT             = error "convertType: cannot convert unit type"

convertLevel :: Level -> ILLevel
convertLevel Zero               = Thread
convertLevel (Step Zero)        = Block
convertLevel (Step (Step Zero)) = Grid
convertLevel l = error ("cannot convert level: " ++ show l)

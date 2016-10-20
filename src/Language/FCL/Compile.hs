module Language.FCL.Compile (compileKernel, compileKernels) where

import qualified Data.Map as Map
import Control.Monad (liftM, (<=<))

import Language.FCL.SourceRegion
import Language.FCL.Syntax
import CGen
--import Language.CGen.Syntax (IExp(IntE))

data Array a = ArrPull { arrayLen :: CExp
                       , arrayElemType :: CType
                       , arrayFun :: CExp -> CGen () a
                       }
             | ArrPush Level (Array a)
             | ArrScanl (Tagged -> Tagged -> CGen () Tagged) Tagged CType (Array a)
             | ArrInterleave Level CExp (Tagged -> CGen () Tagged) (Array a)

mapArray :: (a -> CGen () b) -> CType -> Array a -> Array b
mapArray f outType (ArrPull len _ g) = ArrPull len outType (f <=< g)
mapArray f outType (ArrPush lvl arr) = ArrPush lvl (mapArray f outType arr)
mapArray f outType (ArrInterleave lvl n ixt arr) = ArrInterleave lvl n ixt (mapArray f outType arr)

data Tagged = TagInt CExp
            | TagBool CExp
            | TagDouble CExp
            | TagArray (Array Tagged)
            | TagFn (Tagged -> CGen () Tagged)
            | TagPair Tagged Tagged

instance Show (Array a) where
  show (ArrPull _ _ _) = "<<pull-array>>"
  show (ArrPush lvl arr) = "push<" ++ show lvl ++ "> (" ++ show arr ++ ")"
  show (ArrInterleave lvl e _ arr) = "interleave<" ++ show lvl ++ "> (" ++ show e ++ ") <fn> (" ++ show arr ++ ")"
  show (ArrScanl _ e _ arr) = "scanl <fn> (" ++ show e ++ ") (" ++ show arr ++ ")"

instance Show Tagged where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagArray e) = "TagArray: " ++ show e
  show (TagFn _) = "TagFn"
  show (TagPair e0 e1) = "TagPair(" ++ show e0 ++ ", " ++ show e1 ++ ")"

type VarEnv = Map.Map Name Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

compileKernels :: Int -> Program Type -> [TopLevel]
compileKernels optIterations = map (compileKernel optIterations)

compileKernel :: Int -> Definition Type -> TopLevel
compileKernel optIterations def =
  let e = defBody def
      kernel_name = defVar def
      kernel_body = compile 0 emptyEnv (typeOf e) e
      config = defKernelConfig def
  in fst (generateKernel () optIterations kernel_name kernel_body (configBlockSize config) (configWarpSize config))

addArgument :: Type -> CGen () Tagged
addArgument (PullArrayT bty) =
  do arrVar <- addParam "arrInput" (pointer [attrGlobal] (convertType bty))
     lenVar <- addParam "lenInput" int
     return (tagArray bty (var lenVar) arrVar)
addArgument (PushArrayT _ _) = error "Push arrays can not be kernel arguments"
addArgument ty =
  do v <- addParam "input" (convertType ty)
     return (tagExp ty (var v))

compile :: Int -> VarEnv -> Type -> Exp Type -> CGen () ()
compile i env (ty :> ty') e =
  do taggedExp <- addArgument ty
     let varName = "argument" ++ show i
     compile (i+1) (Map.insert varName taggedExp env) ty' (App e (Var varName ty Missing))
compile _ env _ e = do
  body <- compBody env e
  case body of
    TagInt i -> do
      varOut <- addParam "output" int
      assign varOut i
    TagBool b -> do
      varOut <- addParam "output" bool
      assign varOut b
    TagDouble d -> do
      varOut <- addParam "output" double
      assign varOut d
    TagFn _ -> error "compileFun: Cannot return functions"
    TagPair _ _ -> error "compileFun: TODO return pairs" -- TODO
    TagArray arr -> do
      varOut <- addParam "arrOutput" (pointer [attrGlobal] (baseType arr))
      let writer tv i =
            case tv of
              TagInt v -> assignArray varOut v i
              t -> error (show t)
      forceTo writer arr

tagArray :: Type -> CExp -> VarName -> Tagged
tagArray (PullArrayT _) _ _   = error "Kernels can not accept arrays of arrays"
tagArray (PushArrayT _ _) _ _ = error "Kernels can not accept arrays of arrays"
tagArray (_ :> _) _ _         = error "Kernels can not accept arrays of functions"
tagArray (_ :*: _) _ _        = error "Kernels can not accept arrays of tuples"
tagArray _ len v = TagArray (pullFrom v len)

tagExp :: Type -> CExp -> Tagged
tagExp IntT e    = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e   = TagDouble e
tagExp t _       = error ("tagExp: " ++ show t)

convertType :: Type -> CType
convertType IntT              = int
convertType DoubleT           = double
convertType BoolT             = bool
convertType (PullArrayT ty)   = pointer [] (convertType ty)
convertType (PushArrayT _ ty) = pointer [] (convertType ty)
convertType (_ :> _)          = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :-> _)         = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _)         = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _)          = error "convertType: All type variables should have been resolved by now"

compBody :: Map.Map Name Tagged -> Exp Type -> CGen () Tagged
compBody _ (IntScalar i _)    = return (TagInt (constant i))
compBody _ (DoubleScalar d _) = return (TagDouble (constant d))
compBody _ (BoolScalar b _)   = return (TagBool (constant b))
compBody env (App e0 e1) = do
  v0 <- compBody env e0
  case v0 of
    TagFn f -> f =<< compBody env e1
    _ -> error "Unexpected value at function position in application"
compBody env (Pair e0 e1 _) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  return (TagPair v0 v1)
compBody env (Proj1E e reg) = do
  v <- compBody env e
  case v of
    TagPair v0 _ -> return v0
    _ -> error (show reg ++ ": fst expects a pair as argument")
compBody env (Proj2E e reg) = do
  v <- compBody env e
  case v of
    TagPair _ v1 -> return v1
    _ -> error (show reg ++ ": snd expects a pair as argument")
compBody env (Var x _ reg) =
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error (show reg ++ ": Variable not defined: " ++ x)
compBody env (Lamb x _ e _ _) =
  return . TagFn $ \v ->
    do --v' <- lets "v" v
       compBody (Map.insert x v env) e
compBody env (LambLvl _ e _ _) = compBody env e
compBody env (AppLvl e _) = compBody env e
compBody env (Let x e0 e1 _ _) = do
  v0 <- compBody env e0
  x0 <- lets x v0
  compBody (Map.insert x x0 env) e1
compBody env (UnOp op e0 reg) = do
  v0 <- compBody env e0
  return (compileUnOp op v0 reg)
compBody env (BinOp op e0 e1 reg) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  return (compileBinOp op v0 v1 reg)
compBody env (Cond e0 e1 e2 _ reg) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  case v0 of
    (TagBool b0) -> case (v1, v2) of
                     (TagInt i1, TagInt i2) -> return . TagInt $ if_ b0 i1 i2
                     (TagBool b1, TagBool b2) -> return . TagBool $ if_ b0 b1 b2
                     (TagArray _, TagArray _) -> error (show reg ++ ": TODO: not possible yet")
                     (TagFn _, TagFn _) -> error (show reg ++ ": TODO: yet to be implemented")
                     (_,_) -> error (show reg ++ ": branches are differing")
    _ -> error "Expecting boolean expression as conditional argument in branch"
compBody env (GeneratePull e0 e1 reg) = do
  let (_ :> ty1) = typeOf e1
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagInt e0', TagFn f) ->
      return . TagArray $ ArrPull { arrayElemType = convertType ty1
                                  , arrayLen = e0'
                                  , arrayFun = \i -> f (TagInt i)
                                  }
    _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (MapPull e0 e1 reg) = map_ env e0 e1 reg
compBody env (MapPush e0 e1 reg) = map_ env e0 e1 reg
compBody env (LengthPull e0 reg) = length_ env e0 reg
compBody env (LengthPush e0 reg) = length_ env e0 reg
compBody env (Index e0 e1 reg) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagArray (ArrPull _ _ idx), TagInt i) -> idx i
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (Force e0 reg) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr -> liftM TagArray (force arr)
    _ -> error (show reg ++ ": force expects array as argument")
compBody env (Push lvl e0 _) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr -> return (TagArray (ArrPush lvl arr))
    _ -> error "pull expects array"
compBody env (While e0 e1 e2 _) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  whileArray v0 v1 v2
compBody env (WhileSeq e0 e1 e2 _) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  whileSeq v0 v1 v2
compBody env (Interleave i ixf e0 reg) = do
  vi <- compBody env i
  vixf <- compBody env ixf
  v0 <- compBody env e0
  let PullArrayT (PushArrayT lvl _) = typeOf e0
  case (vi, vixf, v0) of
    (TagInt rn, TagFn ixf', TagArray arr) -> return (TagArray (ArrInterleave lvl rn ixf' arr))
    _ -> error (show reg ++ " Interleave should be given an integer, a function and an array.")
compBody _ (BlockSize _)   = return (TagInt localSize)
compBody env (Scanl e0 e1 e2 _) = do
  v0 <- compBody env e0
  initial <- compBody env e1
  arr <- compBody env e2
  let (_ :> (_ :> returnType)) = typeOf e0
  let op x y =
        case v0 of
          TagFn op' ->
            do op'' <- op' x
               case op'' of
                 TagFn op''' -> op''' y
                 _ -> error ""
          _ -> error ""
  let cty = convertType returnType
  return (TagArray (ArrScanl op initial cty (unArray arr)))


lets :: String -> Tagged -> CGen () Tagged
lets name s =
  case s of
    TagInt x -> liftM TagInt (let_ name int x)
    TagBool x -> liftM TagBool (let_ name bool x)
    TagDouble x -> liftM TagDouble (let_ name double x)
    TagFn _ -> return s
    TagPair x y -> do x' <- lets name x
                      y' <- lets name y
                      return (TagPair x' y')
    TagArray x -> do (n',_) <- letsVar "len" (TagInt (arrayLen x))
                     return (TagArray x {arrayLen = var n'}) -- TODO: materialize??

letsVar :: String -> Tagged -> CGen () (VarName, Tagged)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name int x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name bool x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name double x
                      return (var0, TagDouble (var var0))
    TagFn _ -> error "letsVar TagFn" -- TODO, Impossible - what to do? Just err?
    TagPair _ _ -> error "letsVar TagPair" -- TODO
    TagArray _ -> error "letsVar TagArray" -- TODO

length_ :: Map.Map Name Tagged -> Exp Type -> Region -> CGen () Tagged
length_ env e0 reg = do
  v <- compBody env e0
  case v of
    TagArray arr -> return (TagInt (size arr))
    e -> error (show reg ++ ": Length expects array as argument got " ++ show e)

map_ :: Map.Map Name Tagged -> Exp Type -> Exp Type -> Region -> CGen () Tagged
map_ env e0 e1 reg = do
  let (_ :> outType) = typeOf e0
  f' <- compBody env e0
  e' <- compBody env e1
  case (f', e') of
    (TagFn f, TagArray arr) -> return (TagArray (mapArray f (convertType outType) arr))
    _ -> error $ concat [show reg,
                         ": ",
                         "Map expects function as first argument and",
                         "an array as second argument, got:\n    ",
                         show f',
                         "\nand\n    ",
                         show e']

size :: Array Tagged -> CExp
size (ArrPull len _ _)       = len
size (ArrPush _ arr)         = size arr
size (ArrScanl _ _ _ arr)       = size arr `addi` (constant (1 :: Int))
size (ArrInterleave _ rn _ arr) = rn `muli` size arr

baseType :: Array Tagged -> CType
baseType (ArrPull _ (CPtr _ bty) _)       = bty
baseType (ArrPull _ bty _)       = bty
baseType (ArrPush _ arr)         = baseType arr
baseType (ArrScanl _ _ outTy _)         = outTy
baseType (ArrInterleave _ _ _ arr) = baseType arr

type Writer a = a -> CExp -> CGen () ()

force :: Array Tagged -> CGen () (Array Tagged)
force (ArrPull _ _ _) = error ("force: forcing a pull-array should raise type error." ++
                               "Needs iteration scheme before it can be forced.")
force arr = do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (baseType arr) [attrLocal] len    -- allocate shared memory
  let writer tv i =                      -- creater writer function (right now: only integer arrays supported!)
        case tv of
          TagInt v -> assignArray name v i
          e -> error (show e)
  forceTo writer arr                     -- recursively generate loops for each layer
  return (pullFrom name len)

forceTo :: Writer Tagged -> Array Tagged -> CGen () ()
forceTo writer (ArrPush lvl (ArrPull len _ idx)) = do
  forAll lvl len
    (\i -> do value <- idx i
              writer value i)
forceTo _ (ArrPush _ _) = error "force: push can only be applied to pull-arrays, how did this force appear?"
forceTo writer (ArrInterleave lvl _ f (ArrPull n _ idx)) = do
  distrPar lvl n $ \bix -> do
    arrp <- idx bix
    let writer' a ix =
          do ix' <- f (TagPair (TagInt bix) (TagInt ix))
             writer a (unInt ix')
    case arrp of
      TagArray arrp' -> forceTo writer' arrp'
      _ -> error "Interleave should be applied to an array of arrays!"
forceTo writer (ArrScanl op initial _ (ArrPull n _ idx)) = do
  (tmp_var,tmp) <- letsVar "tmp" initial
  writer initial (constant (0 :: Int))
  for n (\i ->
    do ix <- let_ "i" int (i `addi` (constant (1 :: Int)))
       value <- idx ix
       v <- tmp `op` value
       case v of
         TagInt x -> assign tmp_var x
         TagBool x -> assign tmp_var x
         TagDouble x -> assign tmp_var x
         _ -> error "unsupported type in scanl"
       writer tmp ix)
  return ()

forceTo _ (ArrPull _ _ _)       = error ("force: forcing a pull-array should raise type error." ++
                                         "Needs iteration scheme before it can be forced.")
forceTo _ (ArrInterleave _ _ _ _) = error "force: interleave only accepts pull-arrays. This should have raised a type error."

whileArray :: Tagged -> Tagged -> Tagged -> CGen () Tagged
whileArray (TagFn cond) (TagFn step) (TagArray arr@(ArrPush _ _)) =
  do -- Declare array
     len <- lets "len" (TagInt (size arr))
     var_array <- allocate (baseType arr) [attrLocal] (unInt len)
     (var_len,_) <- letsVar "arraySize" len

     let writer tv i =
           case tv of
             TagInt v -> assignArray var_array v i
             e -> error (show e)

     forceTo writer arr

     let vararr = TagArray (pullFrom var_array (var var_len))

     cond' <- cond vararr
     (var_cond,_) <- letsVar "cond" cond' -- stop condition
     whileLoop (var var_cond) $
       do arr' <- step vararr
          assign var_len (size (unArray arr'))
          -- TODO update arr' length to point to the var_len variable
          forceTo writer (unArray arr')
          cond'' <- liftM unBool (cond arr')
          assign var_cond cond''
     return vararr
whileArray (TagFn _) (TagFn _) _ = error "third argument to while should be a push-array"
whileArray _ _ _ = error "first two arguments to while should be conditional and step function, respectively"

whileSeq :: Tagged -> Tagged -> Tagged -> CGen () Tagged
whileSeq (TagFn cond) (TagFn step) v =
  do (var0, var0tagged) <- letsVar "loopVar" v
     cond' <- cond var0tagged
     case cond' of
       TagBool b -> whileLoop b
                     (do v' <- step var0tagged
                         case v' of
                           TagInt x -> assign var0 x
                           TagBool x -> assign var0 x
                           TagDouble x -> assign var0 x
                           _ -> error "unsupported type in whileSeq")
       _ -> error "Conditional in 'while- should be boolean typed"  
     return v
whileSeq _ _ _ = error "incompatible arguments for whileSeq"


pullFrom :: VarName -> CExp -> Array Tagged
pullFrom name@(_, CPtr _ ty) n =
  ArrPull { arrayElemType = ty
        , arrayLen = n
        , arrayFun = \i -> return (TagInt (name ! i))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
        }
pullFrom _ _ = error "pullFrom: must be applied to pointer-typed variable"

distrPar :: Level -> CExp -> (CExp -> CGen () ()) -> CGen () ()
distrPar (Step (Step Zero)) ub' f =
  do ub <- let_ "ub" int ub'
     q <- let_ "blocksQ" int (ub `divi` numWorkgroups)
     for q (\i -> do
               j <- let_ "j" int ((workgroupID `muli` q) `addi` i)
               f j)
     iff (workgroupID `lti` (ub `modi` numWorkgroups))
         (do j <- let_ "j" int ((numWorkgroups `muli` q) `addi` workgroupID)
             f j
         , return ())
distrPar (Step Zero) ub' f = -- warp level
  do ub <- let_ "ub" int ub'
     numWarps <- let_ "numWarps" int (localSize `divi` warpSize)
     warpsQ <- let_ "warpsQ" int (ub `divi` numWarps)
     warpsR <- let_ "warpsR" int (ub `modi` numWarps)
     lwid <- let_ "lwid" int (localID `divi` warpSize)
     for warpsQ
       (\i -> do warpID <- let_ "warpID" int ((lwid `muli` warpsQ) `addi` i)
                 f warpID)
     iff (lwid `lti` warpsR)
         (do warpID <- let_ "warpID" int ((numWarps `muli` warpsQ) `addi` lwid)
             f warpID
         , return ())
distrPar lvl _ _ = error ("Unsupported level in distrPar: " ++ show lvl)

forAll :: Level -> CExp -> (CExp -> CGen () ()) -> CGen () ()
forAll (Step (Step Zero)) ub' f =
  do ub <- let_ "ub" int ub'
     q <- let_ "q" int (ub `divi` localSize)
     for q (\i -> do v <- let_ "j" int ((i `muli` localSize) `addi` localID)
                     f v)
     iff (localID `lti` (ub `modi` localSize))
       (do v <- let_ "j" int ((q `muli` localSize) `addi` localID)
           f v
       , return ())
     syncLocal
forAll (Step Zero) ub' f =
  do ub <- let_ "ub" int ub'
     q <- let_ "q" int (ub `divi` warpSize)
     r <- let_ "r" int (ub `modi` warpSize)
     wid <- let_ "wid" int (localID `modi` warpSize)
     for q (\i -> do warpID <- let_ "warpID" int ((i `muli` warpSize) `addi` wid)
                     f warpID)
     iff (wid `lti` r)
       (do warpID <- let_ "warpID" int ((q `muli` warpSize) `addi` wid)
           f warpID
       , return ())
     syncLocal
forAll lvl _ _ = error ("Unsupported level in forAll: " ++ show lvl)


compileBinOp :: BinOp -> Tagged -> Tagged -> Region -> Tagged
compileBinOp AddI (TagInt i0) (TagInt i1) _ = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) _ = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) _ = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) _ = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) _ = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) _ = TagInt (mini i0 i1)
compileBinOp EqI  (TagInt i0) (TagInt i1) _ = TagBool (eqi i0 i1)
compileBinOp NeqI (TagInt i0) (TagInt i1) _ = TagBool (neqi i0 i1)
compileBinOp op _ _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])

compileUnOp :: UnOp -> Tagged -> Region -> Tagged
compileUnOp AbsI  (TagInt i0) _ = TagInt (absi i0)
compileUnOp SignI (TagInt i0) _ = TagInt (signi i0)
compileUnOp op _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])

unInt :: Tagged -> CExp
unInt (TagInt i) = i
unInt _          = error "expected int"

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _           = error "expected bool"

unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _            = error "expected array"

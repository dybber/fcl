module Language.FCL.Compile (compileKernel, compileKernels) where

import qualified Data.Map as Map
import Control.Monad (liftM)

import Language.FCL.SourceRegion
import Language.FCL.Syntax hiding (Level)
import qualified Language.FCL.Syntax as FCL
import Language.GPUIL

type PushFn a = ((a -> CExp -> IL ()) -> IL ())

data Idx a = Pull (CExp -> IL a)
           | PushArr Level (PushFn a)

data Array a = Array { arrayLen :: CExp
                     , arrayElemType :: CType
                     , arrayFun :: Idx a
                     }
  deriving Show

data Tagged = TagInt CExp
            | TagBool CExp
            | TagDouble CExp
            | TagArray (Array Tagged)
            | TagFn (Tagged -> IL Tagged)
            | TagPair Tagged Tagged

instance Show (Idx a) where
  show (Pull _) = "<pull array>"
  show (PushArr _ _) = "<push array>"

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

compileKernels :: Int -> Program Type -> [Kernel]
compileKernels optIterations = map (compileKernel optIterations)

compileKernel :: Int -> Definition Type -> Kernel
compileKernel optIterations def =
  let e = defBody def
      kernel_name = defVar def
      kernel_body = compile 0 emptyEnv (typeOf e) e
  in generateKernel optIterations kernel_name kernel_body

addArgument :: Type -> IL Tagged
addArgument (PullArrayT bty) =
  do arrVar <- addParam "arrInput" (pointer [attrGlobal] (convertType bty))
     lenVar <- addParam "lenInput" int
     return (tagArray bty (var lenVar) arrVar)
addArgument (PushArrayT _ _) = error "Push arrays can not be kernel arguments"
addArgument ty =
  do v <- addParam "input" (convertType ty)
     return (tagExp ty (var v))

compile :: Int -> VarEnv -> Type -> Exp Type -> IL ()
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
    TagArray (Array{arrayElemType = CPtr _ _}) -> 
      error "compileFun: cannot return array of arrays"
    TagArray arr -> do
      varOut <- addParam "arrOutput" (pointer [attrGlobal] (arrayElemType arr))
      case arrayFun arr of
        PushArr _ f ->
          f (\x -> case x of
                     TagInt i -> assignArray varOut i
                     t -> error ("Can not return arrays with element type " ++ show t))
        _ -> error "Kernels must return push arrays (at the moment)"

tagArray :: Type -> CExp -> VarName -> Tagged
tagArray (PullArrayT _) _ _ = error "Kernels can not accept arrays of arrays"
tagArray (PushArrayT _ _) _ _ = error "Kernels can not accept arrays of arrays"
tagArray (_ :> _) _ _     = error "Kernels can not accept arrays of functions"
tagArray (_ :*: _) _ _    = error "Kernels can not accept arrays of tuples"
tagArray ty len v =
  TagArray $
    Array { arrayElemType = convertType ty
          , arrayLen = len
          , arrayFun = Pull (\ix -> return (TagInt (v ! ix)))
          }

tagExp :: Type -> CExp -> Tagged
tagExp IntT e = TagInt e
tagExp DoubleT e = TagDouble e
tagExp BoolT e = TagDouble e
tagExp _ _ = error "tagExp"

convertType :: Type -> CType
convertType IntT = int
convertType DoubleT = double
convertType BoolT = bool
convertType (PullArrayT ty) = pointer [] (convertType ty)
convertType (PushArrayT _ ty) = pointer [] (convertType ty)
convertType (_ :> _) = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _) = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _) = error "convertType: All type variables should have been resolved by now"

convertLevel :: FCL.Level -> Level
convertLevel Zero = Thread
convertLevel (Step (Zero)) = Warp
convertLevel (Step (Step Zero)) = Block
convertLevel (Step (Step (Step Zero))) = Grid
convertLevel (Step _) = error "convertLevel: error"
convertLevel (VarL _) = error "convertLevel: All level variables should have been resolved by now"


compBody :: Map.Map Name Tagged -> Exp Type -> IL Tagged
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
      return . TagArray $ Array { arrayElemType = convertType ty1
                                , arrayLen = e0'
                                , arrayFun = Pull (\i -> f (TagInt i))
                                }
    _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (MapPull e0 e1 reg) = do
  let (_ :> ty1) = typeOf e0
  f' <- compBody env e0
  e' <- compBody env e1
  case (f', e') of
    (TagFn f, TagArray (Array n _ idx)) ->
      case idx of
        Pull g -> return $ TagArray $
                    Array { arrayElemType = convertType ty1
                          , arrayLen = n
                          , arrayFun = Pull (\x -> g x >>= f)
                          }
        _ -> error "MapPull received push-array"
    _ -> error $ concat [show reg,
                         ": ",
                         "MapPull expects function as first argument and",
                         "pull array as second argument, got:\n    ",
                         show f',
                         "\nand\n    ",
                         show e']
compBody env (MapPush e0 e1 reg) = do
  let (_ :> ty1) = typeOf e0
  f' <- compBody env e0
  e' <- compBody env e1
  case (f', e') of
    (TagFn f, TagArray (Array n _ idx)) ->
      case idx of
        PushArr lvl g -> return . TagArray $
                    Array { arrayElemType = convertType ty1
                          , arrayLen = n
                          , arrayFun = PushArr lvl (\writer -> g (\e ix -> do v <- f e
                                                                              writer v ix))
                          }
        _ -> error "MapPush received pull-array"
    _ -> error $ concat [show reg,
                         ": ",
                         "MapPush expects function as first argument and",
                         "push array as second argument, got:\n    ",
                         show f',
                         "\nand\n    ",
                         show e']
compBody env (LengthPull e0 reg) = do
  v <- compBody env e0
  case v of
    TagArray arr -> return $ TagInt (arrayLen arr)
    e -> error (show reg ++ ": Length expects array as argument got " ++ show e)
compBody env (LengthPush e0 reg) = do
  v <- compBody env e0
  case v of
    TagArray arr -> return $ TagInt (arrayLen arr)
    e -> error (show reg ++ ": Length expects array as argument got " ++ show e)
compBody env (Index e0 e1 reg) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagArray arr, TagInt i) ->
      case arrayFun arr of
        Pull idx -> idx i
        PushArr _ _ -> do farr <- unsafeWrite arr
                          case arrayFun farr of
                            Pull idx -> idx i
                            PushArr _ _ -> error (show reg ++ ": Impossible")
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (Force e0 reg) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr -> liftM TagArray (unsafeWrite arr)
    _ -> error (show reg ++ ": ComputeLocal expects array as argument")
compBody env (Push lvl e0 _ reg) = do
  v0 <- compBody env e0
--  let (PushArrayT lvl _) = ty
  case v0 of
    TagArray arr -> 
      case arrayFun arr of
        Pull idx -> return (TagArray (arr { arrayFun = PushArr (convertLevel lvl)
                                               (\writer ->
                                                  forAll
                                                    (convertLevel lvl)
                                                    (arrayLen arr)
                                                    (\i -> do value <- idx i
                                                              writer value i))
                                          }))
        PushArr _ _ -> error (show reg ++ ": Input array should be push array")
    _ -> error "Expects array"
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
compBody env (Concat i e0 reg) = do
  vi <- compBody env i
  v0 <- compBody env e0
  let PullArrayT (PushArrayT lvl bty) = typeOf e0
  case (vi, v0) of
    (TagInt rn, TagArray arr) ->
      case arrayFun arr of
        Pull idx -> do
          let n = arrayLen arr
          return . TagArray $ Array { arrayElemType = convertType bty
                                    , arrayLen = n `muli` rn
                                    , arrayFun = 
                                      PushArr (convertLevel lvl)
                                           (\writer -> distrPar Block n $ \bix -> do
                                                arrp <- idx bix
                                                offset <- lets "offset" (TagInt (bix `muli` rn))
                                                let writer' a ix = writer a ((unInt offset) `addi` ix)
                                                case arrp of
                                                  TagArray (Array {arrayFun = PushArr _ p }) -> p writer'
                                                  _ -> error (show reg ++ ": Concat only works with inner-arrays of type push"))
                                    }
        PushArr _ _ -> error "Concat only works when the outer function is a pull array (this should have been checked by the type checker)"
    _ -> error "Concat should be given an integer and an array"
compBody env (Assemble i ixf e0 reg) = do
  vi <- compBody env i
  vixf <- compBody env ixf
  v0 <- compBody env e0
  let PullArrayT (PushArrayT lvl bty) = typeOf e0
  case (vi, vixf, v0) of
    (TagInt rn, TagFn ixf', TagArray arr) ->
      case arrayFun arr of
        PushArr _ _ -> error "Assemble only works when the outer function is a pull array (TODO!)"
        Pull idx -> do
          let n = arrayLen arr
          return . TagArray $ Array { arrayElemType = convertType bty
                                    , arrayLen = n `muli` rn
                                    , arrayFun = 
                                      PushArr (convertLevel lvl)
                                           (\writer -> distrPar (convertLevel lvl) n $ \bix -> do
                                                arrp <- idx bix
                                                let writer' a ix = do ix' <- ixf' (TagPair (TagInt bix) (TagInt ix))
                                                                      case ix' of
                                                                        TagInt ix'' -> writer a ix''
                                                                        _ -> error (show reg ++ " Function argument to Assemble should return an integer value")
                                                case arrp of
                                                  TagArray (Array {arrayFun = PushArr _ p }) -> p writer'
                                                  _ -> error (show reg ++ " Assemble only works with inner-arrays of type push"))
                                    }
    _ -> error (show reg ++ " Assemble should be given an integer, a function and an array.")
compBody _ (LocalSize _)   = return (TagInt localSize)

lets :: String -> Tagged -> IL Tagged
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

letsVar :: String -> Tagged -> IL (VarName, Tagged)
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

getPushFn :: Array a -> PushFn a
getPushFn (Array {arrayFun = PushArr _ f }) = f
getPushFn _ = error "not a push array"

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _ = error "expected bool"
unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _ = error "expected array"

whileArray :: Tagged -> Tagged -> Tagged -> IL Tagged
whileArray (TagFn cond) (TagFn step) (TagArray arr) =
  do -- Declare array
     var_array <- allocate (arrayElemType arr) (arrayLen arr)
     (var_len,_) <- letsVar "arraySize" (TagInt (arrayLen arr))

     -- Materialize before loop
     let f = getPushFn arr
     f (\(TagInt i) -> assignArray var_array i)
     syncLocal

     -- Loop using the same array
     let vararr = arr { arrayFun = Pull (\ix -> return (TagInt (index var_array ix)))
                      , arrayLen = var var_len
                      }
     cond' <- liftM unBool (cond (TagArray vararr))
     (var_cond,_) <- letsVar "cond" (TagBool cond') -- stop condition
     whileLoop (Language.GPUIL.not (var var_cond)) $
       do -- step
          arr' <- liftM unArray (step (TagArray vararr))
          assign var_len (arrayLen arr')
          let new_arr = arr' {arrayLen = var var_len}
          let f' = getPushFn new_arr
          -- materialize before next iteration
          f' (\(TagInt i) -> assignArray var_array i)
          syncLocal
          cond'' <- liftM unBool (cond (TagArray new_arr))
          assign var_cond cond''
     return (TagArray vararr)
whileArray _ _ _ = error "incompatible arguments for while"

whileSeq :: Tagged -> Tagged -> Tagged -> IL Tagged
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

unsafeWrite :: Array Tagged -> IL (Array Tagged)
unsafeWrite arr =
     case arrayFun arr of
       PushArr _ f ->
         do name <- allocate (arrayElemType arr) (arrayLen arr)
            f (\tx ix -> case tx of
                           TagInt v -> assignArray name v ix
                           e -> error (show e))
                    -- TODO ^ This should unpack according elem type, currently only supports integers
                    --        How do we support arrays of arrays?
                    
            syncLocal
            return (pullFrom name (arrayLen arr))
       Pull _ -> error "This should not be possible, array was just converted to push array."

pullFrom :: VarName -> CExp -> Array Tagged
pullFrom name@(_, CPtr _ ty) n =
  Array { arrayElemType = ty
        , arrayLen = n
        , arrayFun = Pull (\i -> return (TagInt (name ! i)))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
        }
pullFrom _ _ = error "pullFrom: must be applied to pointer-typed variable"

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
unInt _ = error "unInt"

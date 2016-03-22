module Language.FCL.Compile (compileKernel, compileKernels) where

import qualified Data.Map as Map
import Control.Monad (liftM)

import Language.FCL.SourceRegion
import Language.FCL.Syntax hiding (Level(..))
import Language.GPUIL

type PushFn a = ((a -> CExp -> IL ()) -> IL ())

data Idx a = Pull (CExp -> IL a)
           | Push (PushFn a)

data Array a = Array { arrayLen :: CExp
                     , arrayElemType :: CType
                     , arrayLevel :: Level
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
  show (Push _) = "<push array>"

instance Show Tagged where
  show (TagInt e) = "TagInt(" ++ show e ++ ")"
  show (TagDouble e) = "TagDouble(" ++ show e ++ ")"
  show (TagBool e) = "TagBool(" ++ show e ++ ")"
  show (TagArray e) = "TagArray: " ++ show e
  show (TagFn _) = "TagFn"
  show (TagPair e0 e1) = "TagPair(" ++ show e0 ++ ", " ++ show e1 ++ ")"

type VarEnv = Map.Map Variable Tagged

emptyEnv :: VarEnv
emptyEnv = Map.empty

compileKernels :: Int -> Program Type -> IO [Kernel]
compileKernels optIterations = mapM (compileKernel optIterations)
 
compileKernel :: Int -> Definition Type -> IO Kernel
compileKernel optIterations def =
  let
    e = defBody def
    name = defVar def
    e' = normalizeFun 0 (typeOf e) e
  in generateKernel optIterations name (compileFun emptyEnv e' >> return ())

-- Convert pull arrays to push arrays
push :: Array a -> Array a
push arr =
  case arrayFun arr of
    Pull idx -> arr { arrayFun = Push (\writer ->
                                          forAll
                                            (arrayLevel arr)
                                            (arrayLen arr)
                                            (\i -> do value <- idx i
                                                      writer value i))
                    }
    Push _ -> arr

normalizeFun :: Int -> Type -> Exp Type -> Exp Type
normalizeFun i (ty :> ty') ebody =
  let varName = ("argument" ++ show i)
      rest = normalizeFun (i+1) ty' (App ebody (Var varName ty Missing))
  in Lamb varName ty rest ty' Missing
normalizeFun _ _ ebody = ebody
  
compileFun :: VarEnv -> Exp Type -> IL ()
compileFun env (Lamb x (ArrayT _ ty) e _ _) = do
  arrVar <- addParam "arrInput" (pointer [attrGlobal] (convertType ty))
  lenVar <- addParam "lenInput" int
  let taggedExp = tagArray ty Block (var lenVar) arrVar
  compileFun (Map.insert x taggedExp env) e
compileFun env (Lamb x ty e _ _) = do
  v <- addParam "input" (convertType ty)
  compileFun (Map.insert x (tagExp ty (var v)) env) e
compileFun env e = do
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
      let f = getPushFn (push arr)
      f (\x -> case x of
                 TagInt i -> assignArray varOut i
                 t -> error ("Can not return arrays with element type " ++ show t))

tagArray :: Type -> Level -> CExp -> VarName -> Tagged
tagArray (ArrayT _ _) _ _ _ = error "Kernels can not accept arrays of arrays"
tagArray (_ :> _) _ _ _     = error "Kernels can not accept arrays of functions"
tagArray (_ :*: _) _ _ _    = error "Kernels can not accept arrays of tuples"
tagArray ty lvl len v =
  TagArray $
    Array { arrayElemType = convertType ty
          , arrayLen = len
          , arrayLevel = lvl
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
convertType (ArrayT _ ty) = pointer [] (convertType ty)
convertType (_ :> _) = error "convertType: functions can not be used as arguments to kernels or occur in arrays"
convertType (_ :*: _) = error "convertType: tuples not yet support in argument or results from kernels (on the TODO!)"
convertType (VarT _) = error "convertType: All type variables should have been resolved by now"

compBody :: Map.Map Variable Tagged -> Exp Type -> IL Tagged
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
compBody env (Generate _ e0 e1 reg) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  case (v0, v1) of
    (TagInt e0', TagFn f) ->
      return . TagArray $ Array { arrayElemType = int -- TODO, when type inference is done,
                                                      -- put something sensible here
                                , arrayLen = e0'
                                , arrayLevel = Block
                                , arrayFun = Pull (\i -> f (TagInt i))
                                }
    _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (Map e0 e1 reg) = do
  f' <- compBody env e0
  e' <- compBody env e1
  case (f', e') of
    (TagFn f, TagArray (Array n ty lvl idx)) ->
      case idx of
        Pull g -> return $ TagArray $
                    Array { arrayElemType = ty -- TODO this is wrong, find the correct return type
                          , arrayLen = n
                          , arrayLevel = lvl
                          , arrayFun = Pull (\x -> g x >>= f)
                          }
        Push g -> return . TagArray $
                    Array { arrayElemType = ty -- TODO this is wrong
                          , arrayLen = n
                          , arrayLevel = lvl
                          , arrayFun = Push (\writer -> g (\e ix -> do v <- f e
                                                                       writer v ix))
                          }
    _ -> error $ concat [show reg,
                         ": ",
                         "Map expects function as first argument and",
                         "pull array as second argument, got:\n    ",
                         show f',
                         "\nand\n    ",
                         show e']
compBody env (Length e0 reg) = do
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
        Push _ -> do farr <- unsafeWrite arr
                     case arrayFun farr of
                          Pull idx -> idx i
                          Push _ -> error (show reg ++ ": Impossible")
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (ForceLocal e0 reg) = do
  v0 <- compBody env e0
  case v0 of
    TagArray arr -> liftM TagArray (unsafeWrite arr)
    _ -> error (show reg ++ ": ComputeLocal expects array as argument")
compBody env (While e0 e1 e2 _) = do
  v0 <- compBody env e0
  v1 <- compBody env e1
  v2 <- compBody env e2
  while v0 v1 v2
compBody env (Concat i e0 reg) = do
  vi <- compBody env i
  v0 <- compBody env e0
  case (vi, v0) of
    (TagInt rn, TagArray arr) ->
      case arrayFun arr of
        Push _ -> error "concat only works when the outer function is a pull array (TODO!)"
        Pull idx -> do
          let n = arrayLen arr
          return . TagArray $ Array { arrayElemType = arrayElemType arr
                                    , arrayLen = n `muli` rn
                                    , arrayLevel = arrayLevel arr
                                    , arrayFun = 
                                      Push (\writer -> distrPar Block n $ \bix -> do
                                                arrp <- idx bix
                                                let writer' a ix = writer a ((bix `muli` rn) `addi` ix)
                                                case arrp of
                                                  TagArray arrp' ->
                                                    case push arrp' of
                                                      (Array {arrayFun = Push p }) -> p writer'
                                                      _ -> error (show reg ++ ": Concat only works with inner-arrays of type push")
                                                  _ -> error (show reg ++ ": Should not happen"))
                                    }
    _ -> error "Concat should be given an integer and an array"
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
getPushFn (Array {arrayFun = Push f }) = f
getPushFn _ = error ""

unBool :: Tagged -> CExp
unBool (TagBool e) = e
unBool _ = error "expected bool"
unArray :: Tagged -> Array Tagged
unArray (TagArray e) = e
unArray _ = error "expected array"

while :: Tagged -> Tagged -> Tagged -> IL Tagged
while (TagFn cond) (TagFn step) (TagArray arr) =
  do -- Declare array
     var_array <- allocate (arrayElemType arr) (arrayLen arr)
     (var_len,_) <- letsVar "arraySize" (TagInt (arrayLen arr))

     -- Materialize before loop
     let f = getPushFn (push arr)
     f (\(TagInt i) -> assignArray var_array i)
     syncLocal

     -- Loop using the same array
     let vararr = arr { arrayFun = Pull (\ix -> return (TagInt (index var_array ix)))
                      , arrayLen = var var_len
                      }
     cond' <- liftM unBool (cond (TagArray vararr))
     (var_cond,_) <- letsVar "cond" (TagBool cond')
     whileLoop (Language.GPUIL.not (var var_cond)) $
       do -- step
          arr' <- liftM unArray (step (TagArray vararr))
          assign var_len (arrayLen arr')
          let new_arr = push (arr' {arrayLen = var var_len})
          let f' = getPushFn new_arr
          -- materialize before next iteration
          f' (\(TagInt i) -> assignArray var_array i)
          syncLocal
          cond'' <- liftM unBool (cond (TagArray new_arr))
          assign var_cond cond''
     return (TagArray vararr)
while (TagFn cond) (TagFn step) v =
  do (var0, var0tagged) <- letsVar "loopVar" v
     cond' <- cond var0tagged
     case cond' of
       TagBool b -> whileLoop b
                     (do v' <- step var0tagged
                         case v' of
                           TagInt x -> assign var0 x
                           TagBool x -> assign var0 x
                           TagDouble x -> assign var0 x
                           _ -> error "")
       _ -> error "Conditional in 'while- should be boolean typed"  
     return v
while _ _ _ = error ""

-- TODO: What will happen with arrays of arrays?
--
-- TODO: Maybe we could just handle everything about array-of-tuples,
-- to tuples of arrays here?
unsafeWrite :: Array Tagged -> IL (Array Tagged)
unsafeWrite arr =
  let parr = push arr
  in case arrayFun parr of
       Push f -> do name <- allocate (arrayElemType parr) (arrayLen parr)
                    f (\tx ix -> case tx of
                                TagInt v -> assignArray name v ix
                                e -> error (show e))
                    -- TODO ^ This should unpack according elem type, currently only supports integers
                    --        How do we support arrays of arrays?
                    
                    syncLocal
                    return (pullFrom name (arrayLen parr) (arrayLevel parr))
       Pull _ -> error "This should not be possible, array was just converted to push array."

pullFrom :: VarName -> CExp -> Level -> Array Tagged
pullFrom name@(_, CPtr _ ty) n lvl =
  Array { arrayElemType = ty
        , arrayLen = n
        , arrayLevel = lvl
        , arrayFun = Pull (\i -> return (TagInt (name ! i)))
                                         -- TODO ^ This should pack
                                         -- according elem type (ty),
                                         -- currently only supports
                                         -- integers
        }
pullFrom _ _ _ = error "pullFrom: must be applied to pointer-typed variable"

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

-- unInt :: Tagged -> CExp
-- unInt (TagInt i) = i
-- unInt _ = error "unInt"
